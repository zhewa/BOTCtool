# Reopen this file with UTF-8 encoding!
# 复制到控制台运行下一行
Sys.setlocale("LC_ALL", "Chinese")

# 血染钟楼(灾祸滋生/灾祸之酿/暗流涌动)完全中立说书人助手
# 玩家人数
n <- 15

# 分配身份 (控制台输入dt可查看所有人身份)
source("./assign_identities.R", encoding = "UTF-8")
source("./night1_functions.R", encoding = "UTF-8")
source("./day_functions.R", encoding = "UTF-8")
source("./night2_beyond_functions.R", encoding = "UTF-8")

# ===============第一晚===============
message("第一晚")
# 顺序：投毒者 洗衣妇 图书管理员 调查员 厨师 共情者 占卜师 管家 间谍
# 投毒者行动后更改poisn的值，若没有投毒者则poisn值不变
poisn <- 9
dt <- tdzf(poisn, dt)
# 洗衣妇 图书管理员 调查员 厨师 共情者 不需要玩家操作，直接分配
xyff(dt, drunknum, townsfolk) # 洗衣妇
tsglyf(dt, drunknum, outsiders) # 图书管理员
dcyf(dt, drunknum, minions) # 调查员
csf(dt, drunknum, sheet) # 厨师
gqzf(dt, drunknum) # 共情者
# 占卜师
# 第一个选的人
zbs1 <- 7
# 第二个选的人
zbs2 <- 1
zbsf(zbs1, zbs2, dt, drunknum, fakedemonnum)
# 管家选的数字 0代表没有选择 -1代表没有管家
gjnum <- 0
gjf(dt, gjnum)
# 间谍
# ===============第一晚结束===============


# ===============白天===============
message("白天")
# 处决数字
cjnum <- 1
dt <- death(dt, cjnum)
message(cjnum, "号玩家", dt[playernum == cjnum, playerid], "被处决")
cjf(dt, cjnum) # 检查小恶魔 圣徒
# ===============白天结束===============


# ===============第二晚及之后的夜晚===============
message("第二晚及之后的夜晚")
# 顺序：投毒者 僧侣 小恶魔 守鸦人 共情者 占卜师 管家 送葬者 间谍
# 执行第一晚的投毒者命令
# 僧侣守卫的数字，-1代表没有僧侣，0代表没有守卫
slnum <- -1
slf(dt, drunknum, slnum)
# 小恶魔杀死的玩家数字，0代表没有杀人
xemnum <- 3
xemf(dt, xemnum)
# 守鸦人
# 若守鸦人死亡，守鸦人选择的数字
syrnum <- 0
syrf(dt, drunknum, syrnum)
# 共情者 占卜师 管家 执行第一晚的命令
# 送葬者
szzf(dt, drunknum, cjnum)
# 间谍
# ===============第二晚及之后的夜晚===============

# Author: Zhe Wang
# Date Created: 20220409
# Email: zhe@bu.edu