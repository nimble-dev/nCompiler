#ifndef _NC_RBROWSER_H_
#define _NC_RBROWSER_H_

// variadic template magic (especially)
// supported by chatGPT

template <typename... Args>
void Rbrowser(const std::vector<std::string>& view_names,
              const std::vector<std::string>& update_names,
              Args&... args) {
    size_t num_view = view_names.size();
    size_t num_update = update_names.size();
    if (sizeof...(Args) != (num_view + num_update)) {
        Rcpp::Rcerr << "Error in Rbrowser: number of names does not match number of arguments." << std::endl;
        return;
    }
    Rcpp::List view_list(num_view);
    Rcpp::List update_list(num_update);
    int i = 0;
    ((i < num_view ? (view_list[i++] = Rcpp::RObject(Rcpp::wrap(args))) : Rcpp::RObject()), ...);
    view_list.attr("names") = view_names;
    // Populate update_list with the next num_update arguments
    int j = 0;
    ((j < num_view ? (j++, Rcpp::RObject()) :
        (j - num_view < num_update ? (update_list[j++ - num_view] = Rcpp::RObject(Rcpp::wrap(args))) : Rcpp::RObject())), ...);
    update_list.attr("names") = update_names;

    Rcpp::Environment nc = Rcpp::Environment::namespace_env("nCompiler");
    Rcpp::Function browser = nc["Rbrowser_fromC"];
    Rcpp::List new_update_list = browser(view_list, update_list);
    
    // Copy updated values from update_list back into the last num_update args
    int k = 0;
    ((k < num_view ? (k++, true) :
        (k - num_view < num_update ?
         (args = Rcpp::as<Args>(new_update_list[k++ - num_view]), true) : false)), ...);
    return;
}

#endif