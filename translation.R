translations <- list(
  en = list(
    title = "CPHI Japan Exhibition Analysis Dashboard",
    subtitle = "Comprehensive Analysis of Asia's Leading Pharmaceutical Event",
    about = "About",
    key_highlights = "Key Highlights",
    exhibitors = "Exhibitors",
    visitors = "Visitors",
    countries = "Countries",
    growth_rate = "Growth Rate"
    # ... add all text elements
  ),
  jp = list(
    title = "CPHI Japan 展示会分析ダッシュボード",
    subtitle = "アジア有数の医薬品イベントの包括的分析",
    about = "概要",
    key_highlights = "主なハイライト",
    exhibitors = "出展者",
    visitors = "来場者",
    countries = "国",
    growth_rate = "成長率"
    # ... add all Japanese translations
  )
)

# Function to get translation
t <- function(key, lang = "en") {
  translations[[lang]][[key]]
}
