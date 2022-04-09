# Author: Zhe Wang
# Date Created: 20220408_144332
# Email: zhe@bu.edu

# Reopen this file with UTF-8 encoding!

# 复制到控制台运行下一行
Sys.setlocale("LC_ALL", "Chinese")
#library(data.table)

# 白天
# 若小恶魔被处决
# cjnum <- 1
# message(cjnum, "号玩家", dt[playernum == cjnum, playerid], "被处决")

cjf <- function(dt, cjnum) {
    if (cjnum == dt[playerid == "小恶魔", as.numeric(playernum)]) {
        if ("荡妇" %in% dt$playerid & nrow(dt[dead == 0, ]) >= 5) {
            message(dt[playerid == "荡妇", playernum], "号玩家(荡妇)成为小恶魔")
        } else {
            message("小恶魔死亡，好人获胜！")
        }
    }

    # 若圣徒被处决
    if (any(grepl("圣徒", dt$playerid))) {
        if (cjnum == dt[grep("圣徒", dt$playerid), as.numeric(playernum)]) {
            message("圣徒死亡，坏人获胜！")
        }
    }
}
