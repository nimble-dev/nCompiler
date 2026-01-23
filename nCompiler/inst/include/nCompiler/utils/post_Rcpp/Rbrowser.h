#ifndef _NC_RBROWSER_H_
#define _NC_RBROWSER_H_

// variadic template magic (especially)
// supported by chatGPT

template <typename... Args>
void Rbrowser(const std::vector<std::string>& names,
              Args&&... args) {
    Rcpp::List view_list(sizeof...(Args));
    int i = 0;
    ((view_list[i++] = Rcpp::RObject(Rcpp::wrap(std::forward<Args>(args)))), ...);
    view_list.attr("names") = names;
    Rcpp::Environment nc = Rcpp::Environment::namespace_env("nCompiler");
    Rcpp::Function browser = nc["Rbrowser_fromC"];
    browser(view_list);
    return;
}

#endif