x_val_lazy <- lazyeval::as.lazy("TIME")
y_val_lazy <- lazyeval::as.lazy("CONC")

lazyeval::interp(~aes(x = x_val, y = y_val, group = ID),
                 x_val = x_val_lazy$expr,
                 y_val = y_val_lazy$expr)


comparisons <- lazyeval::as.lazy("Weight + Age + Conmed")
lazyeval::interp(
    ~lm(data= df, outcome ~ comparisons),
    comparisons = comparisons$expr
)


comparison_list <- list(comparison1 = c("Weight"),
                        comparison2 = c("Weight + Age"),
                        comparison3 = c("Weight + Age + conmed"))

for (i in seq_along(comparison_list)) {
    comparisons <- lazyeval::as.lazy(comparison_list[[i]])
    print(lazyeval::interp(
    ~lm(data= df, outcome ~ comparisons),
    comparisons = comparisons$expr
))

}

