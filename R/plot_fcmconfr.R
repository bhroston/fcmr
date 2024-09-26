
# plot fcmconfr
# bars with error

# Perform visual check
# test_lower_quantile_long <- tidyr::pivot_longer(test_lower_quantile, cols = 2:ncol(test_lower_quantile))
# test_upper_quantile_long <- tidyr::pivot_longer(test_upper_quantile, cols = 2:ncol(test_upper_quantile))
# ggplot() +
#   geom_line(data = test_lower_quantile_long, aes(x = iter, y = value, color = name)) +
#   geom_line(data = test_upper_quantile_long, aes(x = iter, y = value, color = name))

# Perform visual check
# x <- test_bootstrap_noparallel_noprogress
# x <- x[x$node != "A",]
# ggplot() +
#   geom_crossbar(data = x, aes(x = node, y = lower_0.025, ymin = lower_0.025, ymax = upper_0.975), fill = "red", color = "red") +
#   geom_text(data = x, aes(x = node, y = lower_0.025 - 0.05, label = round(lower_0.025, 2))) +
#   geom_text(data = x, aes(x = node, y = upper_0.975 + 0.05, label = round(upper_0.975, 2))) +
#   ylim(0, 1) +
#   theme_classic()
