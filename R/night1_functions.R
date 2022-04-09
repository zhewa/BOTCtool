# Author: Zhe Wang
# Date Created: 20220408_144332
# Email: zhe@bu.edu

# Reopen this file with UTF-8 encoding!

# 复制到控制台运行下一行
Sys.setlocale("LC_ALL", "Chinese")
#library(data.table)

# 第一晚
# 顺序：投毒者 洗衣妇 图书管理员 调查员 厨师 共情者 占卜师 管家 间谍

# message("第一晚")

# 投毒者行动后更改poisn的值
# poisn <- 1

tdzf <- function(poisn, dt) {
    if (any(grepl("投毒者", dt$playerid))) {
        dt[poisoned == 1, poisoned := 0]
        dt[playernum == poisn, poisoned := 1]
        message("投毒者：投毒者毒了",
            poisn, "号玩家(", dt[playernum == poisn, playerid], ")")
    }
    return(dt)
}

# 洗衣妇
xyff <- function(dt, drunknum, townsfolk) {
    if (any(grepl("洗衣妇", dt$playerid))) {
        if (dt[grep("洗衣妇", dt$playerid),
            poisoned == 1 | playernum == drunknum]) {
            xyfnum <- sort(sample(n, 2, replace = FALSE))
            xyfid <- sample(townsfolk, 1, replace = FALSE)
        } else {
            xyfid <- sample(dt[group == "村民" | playerid == "间谍",
                playerid], 1, replace = FALSE)

            if (xyfid == "间谍") {
                if (sample(2, 1) == 1) {
                    xyfnum1 <- dt[playerid == xyfid, playernum]
                    xyfnum2 <- sample(dt[playerid != xyfid, playernum],
                        1, replace = FALSE)
                    xyfid <- paste0(sample(townsfolk, 1, replace = FALSE),
                        "(间谍)")
                } else {
                    xyfnum1 <- dt[playerid == xyfid, playernum]
                    xyfid <- sample(dt[group == "村民",
                        playerid], 1, replace = FALSE)
                    xyfnum2 <- dt[playerid == xyfid, playernum]
                }
                xyfnum <- sort(as.numeric(c(xyfnum1, xyfnum2)))
            } else {
                xyfnum1 <- dt[playerid == xyfid, playernum]
                xyfnum2 <- sample(dt[playerid != xyfid, playernum],
                    1, replace = FALSE)
                xyfnum <- sort(as.numeric(c(xyfnum1, xyfnum2)))
            }
        }
        message("洗衣妇：", xyfnum[1], "号和", xyfnum[2], "号玩家中有", xyfid)
    }
}

# 图书管理员
tsglyf <- function(dt, drunknum, outsiders) {
    if (any(grepl("图书管理员", dt$playerid))) {
        if (dt[grep("图书管理员", dt$playerid),
            poisoned == 1 | playernum == drunknum]) {
            if (sample(choose(n, 2) + 1, 1) == 1) {
                message("图书管理员：本局游戏没有外来者")
            } else {
                tsglynum <- sort(sample(n, 2, replace = FALSE))
                tsglyid <- sample(outsiders, 1, replace = FALSE)
                message("图书管理员：", tsglynum[1],
                    "号和", tsglynum[2], "号玩家中有", tsglyid)
            }
        } else {
            if (nrow(dt[group == "外来者", ]) == 0) {
                # 在没有外来者的情况下，
                # 间谍被图书管理员认成外来者的概率应该是0.5吗？
                if ("间谍" %in% dt$playerid & sample(2, 1) == 1) {
                    tsglynum1 <- dt[playerid == "间谍", playernum]
                    tsglynum2 <- sample(dt[playerid != "间谍", playernum],
                        1, replace = FALSE)
                    tsglyid <- paste0(sample(outsiders, 1, replace = FALSE),
                        "(间谍)")
                    tsglynum <- sort(as.numeric(c(tsglynum1, tsglynum2)))
                    message("图书管理员：", tsglynum[1],
                        "号和", tsglynum[2], "号玩家中有", tsglyid)
                } else {
                    message("图书管理员：本局游戏没有外来者")
                }
            } else {
                tsglyid <- sample(dt[group == "外来者" | playerid == "间谍",
                    playerid], 1, replace = FALSE)

                if (tsglyid == "间谍") {
                    if (sample(2, 1) == 1) {
                        tsglynum1 <- dt[playerid == tsglyid, playernum]
                        tsglynum2 <- sample(dt[playerid != tsglyid, playernum],
                            1, replace = FALSE)
                        tsglyid <- paste0(sample(outsiders, 1, replace = FALSE),
                            "(间谍)")
                    } else {
                        tsglynum1 <- dt[playerid == tsglyid, playernum]
                        tsglyid <- sample(dt[group == "外来者",
                            playerid], 1, replace = FALSE)
                        tsglynum2 <- dt[playerid == tsglyid, playernum]
                    }
                } else {
                    tsglynum1 <- dt[playerid == tsglyid, playernum]
                    tsglynum2 <- sample(dt[playerid != tsglyid, playernum], 1,
                        replace = FALSE)
                }
                tsglynum <- sort(as.numeric(c(tsglynum1, tsglynum2)))
                message("图书管理员：", tsglynum[1],
                    "号和", tsglynum[2], "号玩家中有", tsglyid)
            }
        }
    }
}

# 调查员
dcyf <- function(dt, drunknum, minions) {
    if (any(grepl("调查员", dt$playerid))) {
        if (dt[grep("调查员", dt$playerid),
            poisoned == 1 | playernum == drunknum]) {
            dcynum <- sort(sample(n, 2, replace = FALSE))
            dcyid <- sample(minions, 1, replace = FALSE)
        } else {
            dcyid <- sample(dt[group == "爪牙" | playerid == "隐士",
                playerid], 1, replace = FALSE)
            if (dcyid == "隐士") {
                if (sample(2, 1) == 1) {
                    dcynum1 <- dt[playerid == dcyid, playernum]
                    dcynum2 <- sample(dt[playerid != dcyid, playernum],
                        1, replace = FALSE)
                    dcyid <- paste0(sample(minions, 1, replace = FALSE),
                        "(隐士)")
                } else {
                    dcynum1 <- dt[playerid == dcyid, playernum]
                    dcyid <- sample(dt[group == "爪牙",
                        playerid], 1, replace = FALSE)
                    dcynum2 <- dt[playerid == dcyid, playernum]
                }
                dcynum <- sort(as.numeric(c(dcynum1, dcynum2)))
            } else {
                dcynum1 <- dt[playerid == dcyid, playernum]
                dcynum2 <- sample(dt[playerid != dcyid, playernum],
                    1, replace = FALSE)
                dcynum <- sort(as.numeric(c(dcynum1, dcynum2)))
            }
        }
        message("调查员：", dcynum[1], "号和", dcynum[2], "号玩家中有", dcyid)
    }
}

# 厨师
csf <- function(dt, drunknum, sheet) {
    if (any(grepl("厨师", dt$playerid))) {
        if (dt[grep("厨师", dt$playerid),
            poisoned == 1 | playernum == drunknum]) {
            # simplified probabilities
            if (any(grepl("^隐士", dt$playerid))) {
                if (sample(2, 1) == 1) {
                    maxpair <- sheet["爪牙"] + sheet["恶魔"] +
                        nrow(dt[grep("^隐士", dt$playerid), ]) - 1
                    if (maxpair > 2) {
                        pairs <- sample(seq(0, 3), 1)
                    } else {
                        pairs <- sample(seq(0, maxpair), 1)
                    }
                } else {
                    maxpair <- sheet["爪牙"] + sheet["恶魔"] - 1
                    pairs <- sample(seq(0, maxpair), 1)
                }
            } else {
                maxpair <- sheet["爪牙"] + sheet["恶魔"] - 1
                pairs <- sample(seq(0, maxpair), 1)
            }
        } else {
            csgroup <- dt$group
            if (any(grepl("^隐士", dt$playerid))) {
                if (sample(2, 1) == 1) {
                    csgroup[grep("^隐士", dt$playerid)] <- "爪牙"
                    csgroup[which(dt$group == "恶魔")] <- "爪牙"
                    pairs <- checkevilpairvector(csgroup)
                } else {
                    csgroup[which(dt$group == "恶魔")] <- "爪牙"
                    pairs <- checkevilpairvector(csgroup)
                }
            } else {
                csgroup[which(dt$group == "恶魔")] <- "爪牙"
                pairs <- checkevilpairvector(csgroup)
            }
        }
        message("厨师：有", pairs, "对邪恶玩家为邻座")
    }
}

# 共情者
gqzf <- function(dt, drunknum) {
    if (any(grepl("共情者", dt$playerid))) {
        if (dt[grep("共情者", dt$playerid),
            poisoned == 1 | playernum == drunknum]) {
            pairs2 <- sample(seq(0, 2), 1)
        } else {
            gqznum <- which(dt[dead == 0, playerid] == "共情者")
            lnow <- length(dt[dead == 0, playerid])
            pairs2 <- 0

            if (any(grepl("^隐士", dt$playerid))) {
                ysnum <- grep("^隐士", dt[dead == 0, playerid])
                if ((abs(ysnum - gqznum) == 1 | (ysnum == 1 & gqznum == lnow) |
                        (ysnum == lnow & gqznum == 1)) & sample(2, 1) == 1) {
                    pairs2 <- pairs2 + 1
                }
            }

            if (any(grepl("^间谍", dt$playerid))) {
                jdnum <- which(dt[dead == 0, playerid] == "间谍")
                if ((abs(jdnum - gqznum) == 1 | (jdnum == 1 & gqznum == lnow) |
                        (jdnum == lnow & gqznum == 1)) & sample(2, 1) == 1) {
                    pairs2 <- pairs2 + 1
                }
            }

            minionnums <- which(dt[dead == 0, group] %in% c("恶魔",
                "爪牙") & dt[dead == 0, playerid] != "间谍")
            pairs2 <- pairs2 + sum(abs(minionnums - gqznum) == 1)

            if (gqznum == 1 & any(minionnums == lnow)) {
                pairs2 <- pairs2 + 1
            }

            if (gqznum == lnow & any(minionnums == 1)) {
                pairs2 <- pairs2 + 1
            }
        }
        message("共情者：你左右两侧最靠近你的存活玩家有", pairs2, "个是邪恶的")
    }
}

# 占卜师
# 第一个选的人
# zbs1 <- 0
# 第二个选的人
# zbs2 <- 0

# 不要更改
zbsf <- function(zbs1, zbs2, dt, drunknum, fakedemonnum) {
    zbsflag <- 0

    if (any(grepl("占卜师", dt$playerid))) {
        if (dt[grep("占卜师", dt$playerid),
            poisoned == 1 | playernum == drunknum]) {
            if (sample(2, 1) == 1) {
                zbsflag <- 1
            }
        } else {
            if (zbs1 == fakedemonnum | zbs2 == fakedemonnum) {
                zbsflag <- 1
            }

            if (any(grepl("^隐士", dt$playerid))) {
                ysnumstart <- as.numeric(dt[grep("^隐士", dt$playerid),
                    playernum])
                if (sample(2, 1) == 1 &
                        (zbs1 == ysnumstart | zbs2 == ysnumstart)) {
                    zbsflag <- 1
                }
            }

            emnumstart <- as.numeric(dt[playerid == "小恶魔", playernum])
            if (zbs1 == emnumstart | zbs2 == emnumstart) {
                zbsflag <- 1
            }
        }
        if (zbsflag) {
            message("占卜师：", zbs1, "号和", zbs2, "号里有恶魔")
        } else {
            message("占卜师：", zbs1, "号和", zbs2, "号里没有恶魔")
        }
    }
}

# 管家选的数字
# gjnum <- 0

gjf <- function(dt, gjnum) {
    if (any(grepl("管家", dt$playerid))) {
        if (gjnum > 0) {
            message("管家：选择了", gjnum, "号")
        } else if (gjnum == 0) {
            message("管家：没有选择")
        }
    }
}

# 间谍
