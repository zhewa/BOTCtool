# Author: Zhe Wang
# Date Created: 20220408_144332
# Email: zhe@bu.edu

# Reopen this file with UTF-8 encoding!

# 复制到控制台运行下一行
Sys.setlocale("LC_ALL", "Chinese")

# 第二晚及之后
# 顺序：投毒者 僧侣 小恶魔 守鸦人 共情者 占卜师 管家 送葬者 间谍

# message("第二晚和之后")

# 投毒者
# 投毒者行动后更改poisn的值
# poisn <- 1
# dt[poisoned == 1, poisoned := 0]
# dt[playernum == poisn, poisoned := 1]

# message("投毒者：投毒者毒了",
#     poisn, "号玩家(", dt[playernum == poisn, playerid], ")")

# 僧侣守卫的数字
# slnum <- 0

slf <- function(dt, drunknum, slnum) {
    if (any(grepl("僧侣", dt$playerid))) {
        if (dt[grep("僧侣", dt$playerid),
            poisoned == 1 | playernum == drunknum]) {
            message("僧侣(中毒或醉酒)：守卫的是", slnum, "号玩家，守卫无效")
        } else if (slnum == 0) {
            message("僧侣没有选择守卫玩家")
        } else {
            message("僧侣：守卫的是", slnum, "号玩家")
        }
    }
}

# 小恶魔杀死的玩家数字
# xemnum <- 1

xemf <- function(dt, xemnum) {
    dt <- death(dt, xemnum)
    if (xemnum != 0) {
        message("小恶魔：杀死的是", xemnum, "号玩家")
    } else if (xemnum == 0) {
        message("小恶魔没有选择杀死玩家")
    }

    if (xemnum == dt[playerid == "小恶魔", as.numeric(playernum)]) {

        if ("荡妇" %in% dt$playerid) {
            message(dt[playerid == "荡妇", playernum], "号玩家(荡妇)成为小恶魔")
        } else if (nrow(dt[group == "爪牙", ] > 0)) {
            xemnew <- sample(dt[group == "爪牙", playerid], 1)
            message(dt[playerid == xemnew, playernum], "号玩家(",
                xemnew, ")成为小恶魔")
        } else {
            message("小恶魔死亡，好人获胜！")
        }
    }
}

# 守鸦人
# 若守鸦人死亡，守鸦人选择的数字
# syrnum <- 0

syrf <- function(dt, drunknum, syrnum) {
    if (any(grepl("守鸦人", dt$playerid))) {
        if (dt[grep("守鸦人", dt$playerid), as.numeric(playernum) == syrnum]) {
            message(grep("守鸦人", dt$playerid, value = TRUE), "死亡")
        }

        if (dt[grep("守鸦人", dt$playerid),
            poisoned == 1 | playernum == drunknum]) {
            message(grep("守鸦人", dt$playerid, value = TRUE),
                "(中毒或醉酒)：", syrnum, "号玩家是",
                sample(c(townsfolk, outsiders, minions, demons), 1))
        } else {
            message(grep("守鸦人", dt$playerid, value = TRUE),
                "：", syrnum, "号玩家是", dt[playernum == syrnum, playerid])
        }
    }
}

# 送葬者
szzf <- function(dt, drunknum, cjnum) {
    if (any(grepl("送葬者", dt$playerid))) {
        if (dt[grep("送葬者", dt$playerid),
            poisoned == 1 | playernum == drunknum]) {
            message(grep("送葬者", dt$playerid, value = TRUE),
                "(中毒或醉酒)：", cjnum, "号玩家是",
                sample(c(townsfolk, outsiders, minions, demons), 1))
        } else {
            message(grep("送葬者", dt$playerid, value = TRUE),
                "：", cjnum, "号玩家是", dt[playernum == cjnum, playerid])
        }
    }
}

# 间谍

