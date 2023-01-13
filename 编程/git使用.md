# 文件建立

如果已经存在远程仓库直接使用 `git clone [url]`，建议使用git@.作为远程地址，效率高于https

若现在本地建立，则使用` git init` 创建git文件

若已经在远端建立，使用`git clone [url]`

# 文件修改上传

修改文件后使用`git add [file]`添加文件，`git add .`可以直接添加所有文件。同理使用`git reset [file]`可以撤销文件。

添加修改后使用`git commit -m '[detail]'`用于标记本次更新，如果直接复制对应上次更新则使用`git commit --amend`。注意push后无法再修改。

文件修改后使用`git push`上传，`git pull`下载

- 对接远端时采用`git remote add [remote name] [url]`添加。其中`[remote name]`是设定远端服务器的简称，可以在`git push`时使用
- 在第一次push时使用`git push -u [remote name] [branch name]`是才会默认上传地址，否则每次都需要设置`git push`

# 文件历史查看及回档

采用`git log`可以查看历史文件。`git log -- [file]`可以查看单一文件的历史记录。`git log -2`能查看最近2个记录

`git checkout [hash] [file]`可以回滚文件到某一版本。

`git checkout .`即退回到最新版本

# 分支及合并

`git branch [branch name]`能新建的分支。`git checkout -b [branch name]`能建立切换并新的分支。

- 新的分支下为commit的内容始终存在，commit之后checkout其他分支时，会恢复到其他分支的内容上。
- 此时push也仅会上传本分支下内容，需要通过`git push [remote name] [branch name]`推送

`git merge [branch name]`用于合并分支，需要在合并的主分支内。

- 此时可能存在冲突，但冲突的部分会在原文件中标记，根据标记修改后再次commit即可。
- merge后该分支上的commit历史也会提交到主分支上。

`git branch -d [branch name]`用于删除分支，主要为已合并的分支

- 对于服务器上的分支，需要通过`git push [remote name] :[branch name]`删除


# 冲突解决

pull远程仓库发生冲突时，采用`git stash`暂存，然后`git pull`，采用`git stash pop`会自动合并。

# github连接

## ssh模式

`ssh-keygen -t rsa -C 邮箱`

用于生成新的ssh码。

`ssh -T git@github.com`

验证是否联通

## 帐号密码模式

需要在setting-developer setting中设置token。

`git config --global credential.helper store`用于记录密码



## IP调整

有事出现无法登录的情况。需要调整host的IP。

IP查询网址：[ipaddress.com](https://www.ipaddress.com/)

查询github.com 和 github.global.ssl.fastly.net

在路径`C:\Windows\System32\drivers\etc`下的`host`文件中添加

```
140.82.112.4 github.com
199.232.69.194 github.global.ssl.fastly.net
```

在cmd中输入`ipconfig /flushdns`更新

