#include "message_handler.h"
#include "query_utils.h"

namespace {

/// Trim the spaces and special comment markup at the beginning of the string.
static std::string ltrim(std::string s) {
  auto it = s.begin();
  for (; it != s.end() &&
         (isspace(*it) || *it == '*' || *it == '/' || *it == '!');
       ++it) {
  }

  if (it == s.end())
    return "";
  return std::string(it, s.end());
}

/// Trim the spaces and special comment markup at the end of the string. Leave
/// only one space at the end.
static std::string rtrim(std::string s) {
  char last = '\0';
  if (s.empty())
    return s;
  auto it = std::prev(s.end());
  while ((isspace(*it) || *it == '*' || *it == '/') && it != s.begin()) {
    last = *it;
    --it;
  }

  if (!last)
    return s;
  // Leave one space at the end.
  if (isspace(last))
    ++it;
  return std::string(s.begin(), it);
}

/// Split the string using a provided \p delimiter.
std::vector<std::string> split(const std::string& s, char delimiter) {
  std::vector<std::string> tokens;
  std::string token;
  std::istringstream tokenStream(s);
  while (std::getline(tokenStream, token, delimiter)) {
    tokens.push_back(token);
  }
  return tokens;
}

/// Join strings using a provided \p delimiter.
static std::string join(std::vector<std::string> strings,
                        const char* delimiter) {
  std::ostringstream os;
  bool first = true;
  std::string last;
  for (auto& s : strings) {
    // Skip leading empty lines.
    if (s.empty())
      continue;
    // Do not use a delimiter after newlines.
    if (!first && last != "\n\n") {
      os << delimiter;
    }
    os << s;
    last = s;
    first = false;
  }
  return os.str();
}

/// Format comments for presenting them in a nice way.
/// FIXME: This is a very simplistic approach to parse comments.
/// In the future, a more advanced real comment parser may need
/// to be used.
static std::string format_comments(const std::string& comments) {
  // Split the comments into lines and trim them on the left.
  auto lines = split(comments, '\n');

  // Trim each line on both sides.
  for (auto& line : lines) {
    line = ltrim(line);
    line = rtrim(line);
    if (line.empty())
      line = "\n\n";
  }

  // Remove leading new lines.
  for (auto s = lines.begin(), e = lines.end(); s != e; ++s) {
    if (*s != "\n\n")
      break;
    if (*s == "\n\n")
      *s = "";
  }

  // Remove final new lines.
  for (auto s = lines.rbegin(), e = lines.rend(); s != e; ++s) {
    if (*s != "\n\n")
      break;
    if (*s == "\n\n")
      *s = "";
  }

  // Now merge all lines.
  auto formatted = join(lines, " ");
  return formatted;
}

/// Create documentation string and hover.
/// \returns results are stored in the output parameter \p result.
static void getDocAndHover(const optional<std::string>& hover,
                           const optional<std::string>& comments,
                           const std::string& detailed_name,
                           std::pair<std::string, std::string>& result) {
  result.first = comments ? format_comments(*comments) : "";
  result.second = hover ? *hover : detailed_name;
}

static const optional<std::string>& getCommentOrOverriddenComment(
    QueryDatabase* db,
    QueryFunc& func) {
  if (func.def->comments && !func.def->comments->empty())
    return func.def->comments;
  for (const auto& base : func.def->base) {
    QueryFunc& base_func = db->funcs[base.id];
    if (!base_func.def)
      continue;
    if (base_func.def->comments && !base_func.def->comments->empty()) {
      return base_func.def->comments;
    }
  }
  return func.def->comments;
}

std::pair<std::string, std::string> GetHoverForSymbol(QueryDatabase* db,
                                                      const SymbolIdx& symbol) {
  std::pair<std::string, std::string> docAndHover;
  switch (symbol.kind) {
    case SymbolKind::Type: {
      QueryType& type = db->types[symbol.idx];
      if (type.def)
        getDocAndHover(type.def->hover, type.def->comments,
                       type.def->detailed_name, docAndHover);
      break;
    }
    case SymbolKind::Func: {
      QueryFunc& func = db->funcs[symbol.idx];
      if (func.def)
        getDocAndHover(func.def->hover, getCommentOrOverriddenComment(db, func),
                       func.def->detailed_name, docAndHover);
      break;
    }
    case SymbolKind::Var: {
      QueryVar& var = db->vars[symbol.idx];
      if (var.def)
        getDocAndHover(var.def->hover, var.def->comments,
                       var.def->detailed_name, docAndHover);
      break;
    }
    case SymbolKind::File:
    case SymbolKind::Invalid: {
      assert(false && "unexpected");
      break;
    }
  }
  return docAndHover;
}

struct Ipc_TextDocumentHover : public IpcMessage<Ipc_TextDocumentHover> {
  const static IpcId kIpcId = IpcId::TextDocumentHover;

  lsRequestId id;
  lsTextDocumentPositionParams params;
};
MAKE_REFLECT_STRUCT(Ipc_TextDocumentHover, id, params);
REGISTER_IPC_MESSAGE(Ipc_TextDocumentHover);

struct Out_TextDocumentHover : public lsOutMessage<Out_TextDocumentHover> {
  struct Result {
    std::vector<lsMarkedString> contents;
    optional<lsRange> range;
  };

  lsRequestId id;
  optional<Result> result;
};
MAKE_REFLECT_STRUCT(Out_TextDocumentHover::Result, contents, range);
void Reflect(Writer& visitor, Out_TextDocumentHover& value) {
  REFLECT_MEMBER_START();
  REFLECT_MEMBER(jsonrpc);
  REFLECT_MEMBER(id);
  if (value.result)
    REFLECT_MEMBER(result);
  else {
    // Empty optional<> is elided by the default serializer, we need to write
    // |null| to be compliant with the LSP.
    visitor.Key("result");
    visitor.Null();
  }
  REFLECT_MEMBER_END();
}

struct TextDocumentHoverHandler : BaseMessageHandler<Ipc_TextDocumentHover> {
  void Run(Ipc_TextDocumentHover* request) override {
    QueryFile* file;
    if (!FindFileOrFail(db, request->id,
                        request->params.textDocument.uri.GetPath(), &file)) {
      return;
    }

    WorkingFile* working_file =
        working_files->GetFileByFilename(file->def->path);

    Out_TextDocumentHover out;
    out.id = request->id;

    for (const SymbolRef& ref :
         FindSymbolsAtLocation(working_file, file, request->params.position)) {
      // Found symbol. Return hover.
      optional<lsRange> ls_range = GetLsRange(
          working_files->GetFileByFilename(file->def->path), ref.loc.range);
      if (!ls_range)
        continue;

      // Produce two output strings. One for the documentation and one for the
      // hover containing the singature/description.
      auto docAndHover = GetHoverForSymbol(db, ref.idx);
      if (!docAndHover.first.empty() || !docAndHover.second.empty()) {
        size_t idx = 0;
        out.result = Out_TextDocumentHover::Result();
        if (!docAndHover.first.empty()) {
          out.result->contents.push_back(lsMarkedString());
          out.result->contents[idx].value = docAndHover.first;
          out.result->contents[idx].language = "text";
          ++idx;
        }

        if (!docAndHover.second.empty()) {
          out.result->contents.push_back(lsMarkedString());
          out.result->contents[idx].value = docAndHover.second;
          out.result->contents[idx].language = file->def->language;
          ++idx;
        }

        out.result->range = *ls_range;
        break;
      }
    }

    QueueManager::WriteStdout(IpcId::TextDocumentHover, out);
  }
};
REGISTER_MESSAGE_HANDLER(TextDocumentHoverHandler);
}  // namespace
