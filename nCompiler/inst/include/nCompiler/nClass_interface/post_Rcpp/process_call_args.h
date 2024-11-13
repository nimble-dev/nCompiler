#ifndef NCLASS_PROCESS_CALL_ARGS_H_
#define NCLASS_PROCESS_CALL_ARGS_H_

// There is a forward declaration of process_call_args in generic_class_interface.h

#include <nCompiler/nClass_interface/generic_class_interface.h>
//#include <Rcpp.h>

SEXP process_call_args(const genericInterfaceBaseC::args::argVectorT &argVector,
                       SEXP Sargs) {
  genericInterfaceBaseC::args::argVectorT::size_type numArgsRequired(argVector.size());
  std::vector<std::string> namesProvided(numArgsRequired);
  std::vector<SEXP> promisesProvided(numArgsRequired);
  Rcpp::List RinnerArgs(numArgsRequired);

  // We implement R's rules for argument matching:
  // 1. exact matces; 2. partial matches; 3. positional
  // https://cran.r-project.org/doc/manuals/r-release/R-lang.html#Argument-matching
  // In addition, we do not (cannot easily) support missingness.
  if(numArgsRequired) { // continue if > 0 args
    // Get '...' from the calling environment
    bool using_dotdotdot(true);
    std::vector<int> arg_match_indices(numArgsRequired, -1);
    SEXP Sdotdotdot = Rf_findVarInFrame(Sargs, R_DotsSymbol);
    // std::cout<<TYPEOF(Sdotdotdot)<<std::endl;
    if(TYPEOF(Sdotdotdot) != DOTSXP) {
      //Rcpp::Environment Renv(Sargs);
      //SEXP Sparam = Renv["param"];
      using_dotdotdot = false;
      for(int iReq = 0; iReq < argVector.size(); ++iReq) {
        const std::string expected_name(argVector[iReq].name);
        promisesProvided[iReq] = Rf_findVarInFrame(Sargs, Rf_install(expected_name.c_str()));
        // std::cout<<expected_name<<": "<<TYPEOF(promisesProvided[iReq])<<std::endl;
        if(TYPEOF(promisesProvided[iReq])!=PROMSXP) {
          Rcpp::stop("nCompiler call_method's environment does not have '...' so all arguments must be provided.\n");
        }
      }
    } else {
      // partial_matches "rows" will be required(aka expected), "columns" will be provided
      std::vector<bool> partial_matches(numArgsRequired*numArgsRequired, false);
      std::vector<bool> provided_arg_used(numArgsRequired, false);
      bool done(false);
      // We navigate through the ... pairlist using Rinternals.
      // Use of Rcpp for this was not straightforward.
      SEXP SthisArg = CAR(Sdotdotdot);
      SEXP SnextArg = CDR(Sdotdotdot);
      SEXP SargSym = TAG(Sdotdotdot);
      // Iterate over provided arguments
      size_t numArgsProvided;
      size_t iProv = 0; // index of provided argument
      while(!done) {
        if(TYPEOF(SthisArg)!=PROMSXP) { // PROMSXP=5
          //          std::cout<<"finishing on type "<<TYPEOF(SthisArg)<<std::endl;
          done = true;
        } else {
          promisesProvided[iProv] = SthisArg;
          if(TYPEOF(SargSym)==SYMSXP) { // (SYMSXP=1); so the provided arg is named
            SEXP SprovName = PRINTNAME(SargSym);
            // CHARSXP's have internal details already figured out
            // by Rcpp, so use Rcpp to obtain regular std::string
            const std::string provName(Rcpp::String(SprovName).get_cstring());
            namesProvided[iProv] = provName; // used later only for error reporting.
            // Iterate over require arguments
            bool this_exact_matches(false);
            int iReq = 0;
            while((!this_exact_matches) && iReq < numArgsRequired) {
              // C++ STL tools for partial matching
              // are not the right fit, and this is so simple,
              // so we'll cook it right here: Determine if
              // the name match is exact, partial, or neither
              const std::string expected_name(argVector[iReq].name);
              std::string::const_iterator iExpected(expected_name.begin());
              const std::string::const_iterator iExpEnd(expected_name.end());
              std::string::const_iterator iProvided(provName.begin());
              const std::string::const_iterator iProvEnd(provName.end());
              bool done_this_match((iExpected == iExpEnd) || (iProvided == iProvEnd));
              if(!done_this_match)
                done_this_match = (*iExpected) != (*iProvided);
              while(!done_this_match) {
                ++iExpected;
                ++iProvided;
                done_this_match = (iExpected == iExpEnd) || (iProvided == iProvEnd);
                if(!done_this_match) {
                  done_this_match = (*iExpected) != (*iProvided);
                }
              }
              // std::cout<<*iExpected<<" "<<*iProvided<<std::endl;
              this_exact_matches = (iProvided == iProvEnd) &&
                (*iExpected) == (*iProvided);
              bool this_partial_matches = iProvided == iProvEnd;
              //              std::cout<<"match result for input "<<provName<<" to "<<expected_name<<" "<<this_exact_matches<<" "<<this_partial_matches<<std::endl;
              if(this_exact_matches) { // exact match
                if(arg_match_indices[iReq]==-1) {
                  //                 std::cout<<"exact match for arg "<<provName<<std::endl;
                  arg_match_indices[iReq] = iProv;
                  provided_arg_used[iProv] = true;
                } else {
                  Rcpp::stop("Multiple exact matches to name " + provName);
                }
              } else if(this_partial_matches) {
                partial_matches[iReq + numArgsRequired*iProv] = true;
              }
              ++iReq;
            } // finish iterating over required args
          } // finish handling case that an arg name was input
            //          int l = LENGTH(STRING_ELT(SargName, 0));
            //         std::string ans(CHAR(STRING_ELT(SargName 0)));
            //         std::cout<<"name is "<<ans<<std::endl;
        } // finish handling that this provided arg
        ++iProv;
        done = TYPEOF(SnextArg) == NILSXP;
        //        std::cout<<"next type "<<TYPEOF(SnextArg)<<std::endl;
        if(iProv >= numArgsRequired && !done) {
          Rcpp::stop("Too many arguments provided.");
          done = true;
        }
        if(!done) {
          SthisArg = CAR(SnextArg);
          SargSym = TAG(SnextArg);
          SnextArg = CDR(SnextArg);
        }
      } //finish iterating over provided args
      numArgsProvided = iProv;// We will have already errored out if too many were provided
      if(numArgsProvided != numArgsRequired) {
        Rcpp::stop("Too few arguments provided.");
      }
      // At this point, we have arg_match_indices set for exact matches
      // and partial_matches matrix set for partial matches
      // Next we choose partial matches for args that lack exact matches
      for(int iReq = 0; iReq < numArgsRequired; ++iReq) {
        if(arg_match_indices[iReq]==-1) {
          int new_match(-1);
          for(int iP = 0; iP < numArgsRequired; ++iP) {
            if(partial_matches[iReq + numArgsRequired*iP]) {
              if(new_match != -1) {
                Rcpp::stop("Multiple partial matches to " + namesProvided[iP]);
              } else {
                new_match = iP;
              }
            }
          }
          if(new_match != -1) {
            arg_match_indices[iReq] = new_match;
            provided_arg_used[new_match] = true;
          }
        }
      }
      // At this point, we have arg_match_indices for both exact and partial matches.
      // Now we check if any unused arguments were named, which is an error.
      for(int iP = 0; iP < numArgsRequired; ++iP) {
        if(!provided_arg_used[iP]) {
          if(namesProvided[iP].size() != 0) {
            Rcpp::stop("Named argument " + namesProvided[iP] + " does not match any formal arguments.");
          }
        }
      }
      // And then we place remaining arguments in order.
      bool done_unnamed_matching(false);
      int i_unmatched_req(0);
      int i_unused_prov(0);
      while(!done_unnamed_matching) {
        while(arg_match_indices[i_unmatched_req] != -1 &&
              i_unmatched_req < numArgsRequired-1) {
          ++i_unmatched_req;
        }
        while(provided_arg_used[i_unused_prov] &&
              i_unused_prov < numArgsRequired-1) {
          ++i_unused_prov;
        }
        //        std::cout<<"i_unmatched_req " <<i_unmatched_req<<" i_unused_prov "<< i_unused_prov<<std::endl;
        // At this point we have already trapped mismatched numbers of
        // arguments and multiple partial matches, so
        // the unnamed matches should work perfectly.
        if(arg_match_indices[i_unmatched_req] == -1 &&
           (!provided_arg_used[i_unused_prov])) {
          //          std::cout<<"unnamed placement of provided "<<i_unused_prov<<" for formal "<<i_unmatched_req<<std::endl;
          arg_match_indices[i_unmatched_req] = i_unused_prov;
          provided_arg_used[i_unused_prov] = true;
        } else {
          done_unnamed_matching = true;
        }
      }
      // std::cout<<"arg_match_indices ";
      // for(int iii = 0; iii < numArgsRequired; ++iii) {
      //   std::cout<<arg_match_indices[iii]<<" ";
      // }
      // std::cout<<std::endl;
      // std::cout<<"ready to construct arg list for inner call"<<std::endl;
      //
      // We use Rcpp tools at this step to get PROTECTion behavior.
      // We are not sure how direct use of PROTECT/UNPROTECT works
      // when inside of the called function there could be an error trap
      // that does not return control here for UNPROTECT.
    }
    for(int iReq = 0; iReq < numArgsRequired; ++iReq) {
      int this_iP = using_dotdotdot ? arg_match_indices[iReq] : iReq;
      switch(argVector[iReq].APtype) {
      case genericInterfaceBaseC::copy:
        {
          RinnerArgs[iReq] = Rf_eval(R_PromiseExpr(promisesProvided[this_iP]),
                                     PRENV(promisesProvided[this_iP]));
          break;
        };
      case genericInterfaceBaseC::ref:
        {
          // In the argument place, put a list of the argument name and its environment
          // This imitates R function createRefInfoIntoC.  Any changes here or there must
          // be kept up to date with each other.
          std::cout<<"handling ref"<<std::endl;
          RinnerArgs[iReq] = Rcpp::List::create(PRCODE(promisesProvided[this_iP]),
                                                PRENV(promisesProvided[this_iP]));
          break;
        };
      case genericInterfaceBaseC::refBlock:
        {
          std::cout<<"handling blockRef"<<std::endl;
          RinnerArgs[iReq] = Rcpp::List::create(PRCODE(promisesProvided[this_iP]),
                                                PRENV(promisesProvided[this_iP]));
          break;
        };
      default:
        Rcpp::stop("Invalid argPassingType"); // should never ever be reached
      }
    }

    // Can I UNPROTECT(1) here, in case there is any error-trap inside the
    // method that prevents returning to this code?
    // In the subsequent calls, there is a LENGTH() and VECTOR_ELT() calls on SinnerArgs.
    // Are those safe without PROTECTion?
    // Alternatively, should I make SinnerArgs an Rcpp::List?
  }         // end of if(numArgsRequired)
  SEXP Sans = RinnerArgs;
//  SEXP Sans = PROTECT(method->second.method_ptr->call(this, RinnerArgs));
//  UNPROTECT(1);
  return Sans;
}


#endif // NCLASS_PROCESS_CALL_ARGS_H_
