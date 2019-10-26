# splitbot [![CircleCI](https://circleci.com/gh/juanedi/splitbot.svg?style=svg)](https://circleci.com/gh/juanedi/splitbot)

This is a Telegram bot to interact with the Splitwise API. This isn't intended for everyone: it was developed to make my personal workflow easier (and to learn a little Haskell while I'm at it).

## Setup

### Development environment

This is a pretty standard [Haskell Stack](https://haskellstack.org) project. After installing Stack, you should be able to fetch the corresponding version of GHC and build the project as follows

```
> stack setup
> stack build
```

You can also run `make build-watch` to build the code and watch for file changes.

### Splitwise token

Please refer to the [Splitwise API docs](http://dev.splitwise.com/) for information on how to register your app and get a token. You only need to do this once.

At the time of writing those docs aren't very good, so you might want to look at the beta version of the upcoming version of the documentation [here](https://dev.beta.splitwise.com). You'll notice there's a handy Ruby script to fetch the token.

### Telegram bot

Please refer to the [Telegram bot API docs](https://core.telegram.org/bots#3-how-do-i-create-a-bot) for instruction on how to create a bot. TL;DR: Talk to `@BotFather` on Telegram, follow its instructions and take note of the token it will give you.

## Configuration

All settings are read from environment variables. Please take a look at the [Settings module](https://github.com/juanedi/splitbot/blob/master/src/Settings.hs) for a complete list of required variables.

## Running

Executing `stack build` will produce the `splitbot` executable. To run it, you just need to make sure the appropriate settings are defined in the environment and then pass n a command line argument to indicate the desired strategy for fetching telegram updates: `--server` or `--polling`:

### --polling

The program will use [long polling](https://en.wikipedia.org/wiki/Push_technology#Long_polling) as the method to fetch updates. In a nutshell: our program sends an HTTP request to Telegram, which will block and reply only when an update is actually available. As soon as we get a response we make another request to get notified when new messages arrive.

This is the recommended way to use the bot during development since setup is a lot simpler.

### --server

This program gets push notifications from Telegram via webhooks. A web server is started and updates are posted as JSON at the `/#BOT_TOKEN#/updates` endpoint.

Note that you'll need domain with a valid, non-wildcard SSL certificate. The server started by `splitbot` doesn't handle SSL certificates, so you should set up a proxy server (such as Apache or NGINX) on front of it. When doing this, take into account that the Telegram API requires you to present the full certificate chain. You'll find more detailed instructions [here](https://core.telegram.org/bots/webhooks).

After that, register the webhook as described [here](https://core.telegram.org/bots/webhooks).

## Releasing

The `push-release` task in the Makefile will tag the current version and push it to Github. CI should take it from there, building a docker image and publishing it in Docker Hub.
