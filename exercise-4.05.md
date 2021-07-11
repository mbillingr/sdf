The segment matcher loops through all possibilities while any following matcher
indicates failure by returning #f. Since our final "success" procedure prints
the resulting bindings and then returns #f, none of the matches actually
succeed. Thus, all possibilities are generated, and finally the match fails.
