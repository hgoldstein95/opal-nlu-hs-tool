# Opal NLU Type Utility

## How to Use
This tool is still in early stages of development, so it is not as user friendly
as it could be. That being said, it gets the job done.

### 1. Write your types.
The `example/test.wit.opal` file is a good starting point. Write down any types
that your applicatoin needs.

### 2. Run the tool.
Run the Haskell tool using
```
stack run -- -- <path-to-types>
```

### 3. Transfer typescript declarations.
The first thing the tool outputs is a number of typescript declarations. Pull
these into your project.

### 4. Configure Wit.
Using `example/Template.zip` as a guide, set up your Wit application. Change
relevant information in the top-level JSON files. Next, add your entity files to
the entity directory. They are generated into `out/<entity>.json`. Finally, use
the `zip-command` in `app.json` to zip the configuration.

### 5. Train your Wit application.
After setting up the app (there will be an option to "restore from backup"; use
that to upload your zip), enter examples to train the NLU engine.
