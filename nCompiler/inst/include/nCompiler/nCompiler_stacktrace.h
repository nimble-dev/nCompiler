#ifndef NCOMPILER_STACKTRACE
#define NCOMPILER_STACKTRACE

// build a crude stack trace of nFunctions.  each level of the trace captures
// the function name in [StringPair].first and the step being executed in
// [StringPair].second.  The step may be updated several times within a single
// function call, as the function executes.
typedef std::pair<std::string, std::string> StringPair;
static std::vector<StringPair> msgs;
static bool nc_did_throw = false;
#define CODE__(...) __VA_ARGS__
#define PUSH_DEBUGFUN(x) { msgs.emplace_back(StringPair(x, "")); }
#define SET_DEBUG_MSG(x, description) { msgs.back().second = description; x; }
#define POP_DEBUGFUN() { msgs.pop_back(); }

#define BEGIN_NC_ERRORTRAP nc_did_throw = false; try {

#define END_NC_ERRORTRAP                                                       \
    }                                                                          \
    catch(const std::exception& e) {                                           \
        if(nc_did_throw) {                                                     \
            throw;                                                             \
        } else {                                                               \
            std::string s;                                                     \
            std::string indent = "";                                           \
            bool firstLine = true;                                             \
            for(const auto &msg : msgs) {                                      \
                s += indent + "Error in " + msg.first + " at \"" +             \
                     msg.second + "\" : \r\n";                                 \
                indent += "  ";                                                \
                if(firstLine) {                                                \
                    indent += "  ";                                            \
                    firstLine = false;                                         \
                }                                                              \
            }                                                                  \
            s += indent + e.what();                                            \
            nc_did_throw = true;                                               \
            msgs.clear();                                                      \
            throw std::runtime_error(s);                                       \
        }                                                                      \
    }

#endif // NCOMPILER_STACKTRACE
