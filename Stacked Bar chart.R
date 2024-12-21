# نصب و بارگذاری بسته‌های مورد نیاز
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# ساخت یک دیتافریم نمونه
data <- data.frame(
  Score = 10:16,
  Warm_water_group = c(2, 3, 6, 3, 9, 6, 1),
  Cold_water_group = c(1, 1, 0, 10, 0, 8, 2),
  Control = c(1, 0, 4, 0, 4, 0, 4)
)

# تبدیل داده ها به فرمت Long
data_long <- pivot_longer(data, cols = -Score, names_to = "Group", values_to = "Value")

# تنظیم ترتیب فاکتورها برای نمایش گروه ها
data_long$Group <- factor(data_long$Group, levels = c("Control", "Cold_water_group", "Warm_water_group"))

# محاسبه جمع کل برای هر Score
data_summary <- data_long %>%
  group_by(Score) %>%
  summarise(Total = sum(Value), .groups = 'drop')

# رسم نمودار افقی با ترتیب گروه های مطلوب
  p <- ggplot(data_long, aes(x = factor(Score), y = Value, fill = Group)) +
  geom_bar(stat = "identity", position = "stack", width = 0.6) +
  geom_text(aes(label = ifelse(Value == 0, '', Value)), position = position_stack(vjust = 0.5), color = "white") +  # نمایش مقادیر داخل میله‌ها با رنگ سفید
  geom_text(data = data_summary, aes(x = factor(Score), y = Total, label = Total, fill = NULL), inherit.aes = FALSE, vjust = 0.5, hjust = -0.5, color = "black", fontface = "bold") +  # نمایش مقادیر کل
  labs(x = "Sleep Quality scores", y = NULL, title = "Pre-Intervention") +
  scale_fill_manual(values = c("Warm_water_group" = "grey20", "Cold_water_group" = "grey45", "Control" = "grey70")) +
  scale_y_continuous(expand = expansion(add = c(0, 1))) +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.y = element_text(face = "bold", size = 13),
        axis.text.y = element_text(size = 13, color = "black", face = "bold"),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 12, face = "bold"),
        plot.title = element_text(face = "bold", size = 14)) +
  coord_flip()

# دانلود نمودار با ggsave
ggsave("high_quality_chart Pre intervention.png", plot = p, width = 10, height = 6, dpi = 300)
