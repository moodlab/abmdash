# abmdash: An R pacakge for creating an automatically updating dashboard

## Setting up dev environment + contributing to this repository

### Clone this repo from GitHub

```         
git clone https://github.com/moodlab/abmdash.git
```

### Set up pre-commit hooks

We use pre-commit hooks to make sure we don't accidentally make changes to the code that make our lives harder. See [this issue](https://github.com/moodlab/abmdash/issues/2) for more description.

The pre-commit hooks will run on the GitHub repository each time we push updates using Github Actions via the file in `.github/workflows/pre-commit.yml`. This will prevent code from being merged if it doesn't pass the checks.

You also only have to do this process the first time you're setting up the repo, after that the pre-commit hooks should be set up and run every time you try to commit locally.

To install pre-commit hooks locally so you can make sure your code can get merged:

#### MacOS

In your terminal (You can use the "Terminal" tab inside of RStudio or Positron OR use spotlight search for "terminal" and open the app that pops up first) run the following code:

```         
## Install homebrew if you haven't already (a package manager like CRAN for MacOS)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
## Install pre-commit with homebrew
brew install pre-commit
```

Then, after navigating inside the `abmddash/` directory, go to your R console and run

```         
## Confirm you're in your local abmdash directory
getwd()
## Install and set up pre-commit hooks locally
install.packages("precommit")
precommit::use_precommit()
```

#### Linux

This can also be done in theory for MacOS

Note: This process should work in theory but hasn't yet been directly tested

In your terminal (You can use the "Terminal" tab inside of RStudio or Positron) run the following code:

```         
## Install uv to handle Python environments and installation
curl -LsSf https://astral.sh/uv/install.sh | sh
## Install pre-commit with uv
uv tool install pre-commit
```

Then, after navigating inside the `abmddash/` directory, go to your R console and run

```         
## Confirm you're in your local abmdash directory
getwd()
## Install and set up pre-commit hooks locally
install.packages("precommit")
precommit::use_precommit()
```

#### Windows

Note: This process should work in theory but hasn't yet been directly tested

In your terminal/powershell run the following code:

```         
## Install uv to handle Python environments and installation
powershell -ExecutionPolicy ByPass -c "irm https://astral.sh/uv/install.ps1 | iex"
## Install pre-commit with uv
uv tool install pre-commit
```

Then, after navigating inside the `abmddash/` directory, go to your R console and run

```         
## Confirm you're in your local abmdash directory
getwd()
## Install and set up pre-commit hooks locally
install.packages("precommit")
precommit::use_precommit()
```

#### What if my pre-commit hooks fail?

For a lot of the sytling ones, the fixes are automatic! You can re-stage everything

```
git add .
```

And re-do your commit message

```
git commit -m "Here's your commit message"
```
If all the checks pass, you're good to go!

Otherwise, the pre-commit hooks are ususally pretty explicit about what has specifically failed and where. Use those error messages to guide any further fixes.

If you really get stuck, you can file an issue with the Github repo with a bug report, including the full text of the pre-commit output + any other necessary context.

### Create an issue with a description of what you're doing

In the Github [repo](https://github.com/moodlab/abmdash/issues) create a new issue with a description of what your code changes will do. If you're not sure where to start with the description you can look at previous issues.

Generally, issues that add a new feature will start with `feat:`, issues that fix a bug will start with `bug:`, and issues that maintain functionality but modify code for other reasons will start with `maint:`.

You can also file an issue without intending to change the code yourself. If that's the case please provide a detailed description of what the functionality is now, what the functionality will be after the code change, and assign the issue to the person you're hoping will make the code change.

### Create a git branch related to the issue you created

From the terminal, use:

```
git switch -c YOUR_GITHUB_USERNMAE/SHORT_DESCRIPTION
```

For example, when I created a branch to add pre-commit hooks to the repo my command was

```
git switch -c mcmullarkey/add-pre-commit
```

Creating a new branch allows us to isolate our work on a particular part of the code. 

The `main` branch of this repo is also protected to prevent code changes that can break the package, so developing on a feature branch allows you to make sure your code passes the necessary checks.

### Make whatever changes you want on your branch and commit them

Go for it!

You'll have to pass the pre-commit checks, but as stated above they do a pretty good job of guidign you if any of them are failing.

The most important part is to include a commit with the text "Closes #{YOUR_ISSUE_NUMBER}" in the commit message

For example, my commit message tends to look something like "Closes #2: blah blah blah"

It's important to include this text so the Github issues automatically get resolved by the pull request later and we don't have to do that step manually.

### Push your branch to GitHub + create a pull request

After all your commits are made, use

```
git push origin YOUR_GITHUB_USERNMAE/SHORT_DESCRIPTION
```

This should be the same name as your local branch for consistency

So for me, when I pushed my branch adding pre-commit checks it looked like

```
git push origin mcmullarkey/add-pre-commit
```

Then, go to the URL provided after your push succeeds to create a pull request.

In the future I'll try to have a pull request template that gets used automatically. For now, you can look at previous pull requests for examples.

### As of now, you can bypass the review for pull requests

This will probably change in the future, but as of right now as long as you wait for the checks to pass you can bypass the review process and self-merge your pull request.

### You did it!

Thanks for contributing to the repo!
