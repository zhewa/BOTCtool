# Author: Zhe Wang
# Date Created: 20220408_144332
# Email: zhe@bu.edu

# Reopen this file with UTF-8 encoding!

# 复制到控制台运行下一行
Sys.setlocale("LC_ALL", "Chinese")
suppressMessages(library(data.table))

# 玩家人数
# n

checktwoevilpair <- function(x, y, term = "爪牙") {
    if (x == y & x == term) {
        return(1)
    } else {
        return(0)
    }
}

checkevilpairvector <- function(vec) {
    l <- length(vec)
    numpair <- 0
    for (i in seq(l - 1)) {
        numpair <- numpair + checktwoevilpair(vec[i], vec[i + 1])
    }
    numpair <- numpair + checktwoevilpair(vec[1], vec[l])
    return(numpair)
}

death <- function(dt, n) {
    dt[playernum == n, dead := 1]
    return(dt)
}

# 血染钟楼
time <- format(Sys.time(), "%m%d%H%M%S")
message("当前日期时间：", time)
nmin <- 5
nmax <- 15
playernums <- seq(nmin, nmax)
group <- c("村民", "外来者", "爪牙", "恶魔")
ids <- vector("list", length = length(group))
names(ids) <- group

# 灾祸之酿/暗流涌动 角色
townsfolk <- c("洗衣妇",  "图书管理员",  "调查员", "厨师", "共情者",
    "占卜师", "送葬者", "僧侣",
    #"守鸦人/渡鸦看守者", "处女/童贞之人",
    "守鸦人", "处女",
    "杀手", "士兵", "市长")
townsfolkleft <- townsfolk
outsiders <- c("管家", "酒鬼", "隐士", "圣徒")
minions <- c("投毒者", "间谍", "荡妇", "男爵")
demons <- c("小恶魔")

ids[[group[1]]] <- townsfolk
ids[[group[2]]] <- outsiders
ids[[group[3]]] <- minions
ids[[group[4]]] <- demons

# player identity layout 板子
playersheet <- vector("list", length = length(playernums))
names(playersheet) <- as.character(playernums)

townsfolkn <- c(3, 3, 5, 5, 5, 7, 7, 7, 9, 9, 9)
outsidersn <- c(0, 1, 0, 1, 2, 0, 1, 2, 0, 1, 2)
minionsn <- c(1, 1, 1, 1, 1, 2, 2, 2, 3, 3, 3)
demonsn <- rep(1, nmax - nmin + 1)

for (i in seq(length(playernums))) {
    layout <- c(townsfolkn[i], outsidersn[i], minionsn[i], demonsn[i])
    names(layout) <- c(group[1], group[2], group[3], group[4])
    playersheet[[as.character(playernums[i])]] <- layout
}

playerids <- vector("character", length = n)
names(playerids) <- seq(n)
unassigned_players <- seq(n)
sheet <- playersheet[[as.character(n)]]
print(sheet)

# 分角色
set.seed(time)

# 恶魔
if (sheet[group[4]] == 0) {
    message("本场游戏没有恶魔")
} else if (sheet[group[4]] > 0) {
    demonnum <- sample(unassigned_players, sheet[[group[4]]], replace = FALSE)
    demonids <- sample(ids[[group[4]]], sheet[[group[4]]],
        replace = FALSE)
    for (i in seq(sheet[[group[4]]])) {
        playerids[demonnum[i]] <- demonids[i]
        #message(demonids[i], "是", demonnum[i], "号玩家")
    }
    unassigned_players <- unassigned_players[-which(unassigned_players %in%
            demonnum)]
} else {
    stop("错误：恶魔数量小于0！")
}

# 爪牙
if (sheet[group[3]] == 0) {
    message("本场游戏没有爪牙")
} else if (sheet[group[3]] > 0) {
    minionsnum <- sample(unassigned_players, sheet[group[3]], replace = FALSE)
    minionsid <- sample(minions, length(minionsnum), replace = FALSE)

    for (i in seq(length(minionsnum))) {
        playerids[minionsnum[i]] <- minionsid[i]
        #message(minionsid[i], "是", minionsnum[i], "号玩家")
    }
    unassigned_players <- unassigned_players[-which(unassigned_players %in%
            minionsnum)]
    realsheet <- sheet

    if ("男爵" %in% minionsid) {
        realsheet[group[2]] <- realsheet[group[2]] + 2
        realsheet[group[1]] <- realsheet[group[1]] - 2
    }
} else {
    stop("错误：爪牙数量小于0！")
}

# 外来者
drunknum <- -1
if (realsheet[group[2]] == 0) {
    message("本场游戏没有外来者")
} else if (realsheet[group[2]] > 0) {
    outsidersnum <- sample(unassigned_players, realsheet[group[2]],
        replace = FALSE)
    outsidersid <- sample(outsiders, length(outsidersnum), replace = FALSE)

    for (i in seq(length(outsidersnum))) {
        playerids[outsidersnum[i]] <- outsidersid[i]
        #message(outsidersid[i], "是", outsidersnum[i], "号玩家")
    }
    unassigned_players <- unassigned_players[-which(unassigned_players %in%
            outsidersnum)]

    if ("酒鬼" %in% outsidersid) {
        drunknum <- outsidersnum[which(outsidersid == "酒鬼")]
        drunkfakeid <- sample(townsfolk, 1, replace = FALSE)
        townsfolkleft <- townsfolk[-which(townsfolk %in% drunkfakeid)]
        # playerids[which(playerids %in% "酒鬼")] <- paste0("酒鬼(",
        #     drunkfakeid, ")")
        #message("酒鬼以为他是", drunkfakeid)
    }
} else {
    stop("错误：外来者数量小于0！")
}

# 村民
if (realsheet[group[1]] == 0) {
    message("本场游戏没有村民")
} else if (realsheet[group[1]] > 0) {
    townsfolknum <- sample(unassigned_players, realsheet[group[1]],
        replace = FALSE)
    townsfolkid <- sample(townsfolkleft, length(townsfolknum), replace = FALSE)

    for (i in seq(length(townsfolknum))) {
        playerids[townsfolknum[i]] <- townsfolkid[i]
        #message(townsfolkid[i], "是", townsfolknum[i], "号玩家")
    }
    unassigned_players <- unassigned_players[-which(unassigned_players %in%
            townsfolknum)]
} else {
    stop("错误：村民数量小于0！")
}

dt <- data.table(playernum = names(playerids),
    playerid = playerids)
dt[playerid %in% townsfolk, group := "村民"]
dt[playerid %in% outsiders, group := "外来者"]
dt[playerid %in% minions, group := "爪牙"]
dt[playerid %in% demons, group := "恶魔"]
if ("酒鬼" %in% dt$playerid) {
    dt[playerid == "酒鬼", playerid := paste0("酒鬼(", drunkfakeid, ")")]
}

if ("占卜师" %in% dt$playerid) {
    fakedemon <- sample(dt[playerid != "占卜师" &
            group %in% c("外来者", "村民"), playerid],
        1, replace = FALSE)
    fakedemonnum <- which(dt$playerid == fakedemon)
    dt[playerid == fakedemon, playerid := paste0(playerid, "(假恶魔)")]
}

print(dt)
#View(dt)

# 恶魔的伪装身份
if (length(townsfolk) - nrow(dt[group == "村民", ]) >= 4) {
    if (any(grepl("酒鬼", dt$playerid))) {
        emptyids <- setdiff(setdiff(townsfolk, playerids), drunkfakeid)
    } else {
        emptyids <- setdiff(townsfolk, playerids)
    }
    emptyids3 <- sample(emptyids, 3, replace = FALSE)
} else {
    emptyids3 <- setdiff(townsfolk, playerids)
}

message("可伪装的身份是：", paste0(emptyids3, collapse = ", "))

dt[, poisoned := 0]
dt[, dead := 0]
