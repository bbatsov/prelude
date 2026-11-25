# Prelude Forge

## Overview

[Forge](https://github.com/magit/forge) is a Magit extension for
working with Git forges - GitHub, GitLab, Gitea, Bitbucket, and
others - without leaving Emacs. With Forge you can:

- List and view pull requests and issues alongside branches and
  commits in the Magit status buffer.
- Read PR/issue threads, post comments and reactions.
- Create new issues and PRs.
- Edit issue/PR titles, labels, assignees, and milestones.
- Track and check out review branches.

Forge stores its data in a local SQLite database, which is populated
on demand when you visit a repository's forge section in Magit.

## Key Bindings

Forge integrates with Magit's existing key bindings - there are no
new global keys. Once a repository is set up:

| Key | Where | Description |
| --- | ----- | ----------- |
| <kbd>'</kbd> | `magit-status` | Forge dispatch menu |
| <kbd>N</kbd> | `magit-status` | Forge sub-menu (older binding) |

The dispatch menu (apostrophe) is the canonical entry point and
exposes everything: pulling topics, browsing PRs/issues, posting,
configuring the repo, etc.

## Setup

### 1. Authentication

Forge needs an API token to talk to your forge. The recommended way
is `auth-source` (typically `~/.authinfo` or `~/.authinfo.gpg`):

```text
machine api.github.com login YOUR-USERNAME^forge password ghp_YOUR_TOKEN_HERE
```

For GitHub, generate a personal access token with `repo` scope at
<https://github.com/settings/tokens>. For GitLab use a personal
access token with `api` scope.

### 2. Add a repository

Open a repo in `magit-status`, then run <kbd>M-x forge-add-repository</kbd>
(or use the dispatch menu). Forge will fetch topics into its local
database - this can take a moment for large repos.

### 3. (Optional) Self-hosted instances

For self-hosted GitLab/Gitea/etc. add an entry to
`forge-alist`:

```emacs-lisp
(with-eval-after-load 'forge
  (add-to-list 'forge-alist
               '("gitlab.example.com"
                 "gitlab.example.com/api/v4"
                 "gitlab.example.com"
                 forge-gitlab-repository)))
```

See the [Forge manual](https://magit.github.io/forge/) for the full
reference.
