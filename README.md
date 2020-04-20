# Shiny Speech

## Goal

Try different TTS (text-to-speech) services in one portal. Currently support `amazon` (Polly), `google` and `microsoft` (Azure) through R package [text2speech](https://github.com/muschellij2/text2speech).

## Usage

Depend on `shiny` to run in RStudio or simply run `Rscript app.R` after installing necessary packages.

Service provider|How to use|Voice availability
-----------------|------------|-------------------
Amazon|Create user for Polly from [console](https://console.aws.amazon.com/iam/) to get the key and secret|Must have a valid account. Voice availability is affected by region, but region switching is not implemented yet. Default region is `us-east-1` in `aws.polly` package.
Google|Create and download JSON format [credential](https://console.cloud.google.com/apis/credentials) to upload|Must have a valid account.
Microsoft|Pick a [region](https://docs.microsoft.com/en-us/azure/cognitive-services/speech-service/regions#text-to-speech) and input key for the region|Voice availability is affected by region, and key is region specific. Leave key blank to view a list of common voices from `mscstts` package.

## Deployment

<https://beta.rstudioconnect.com/content/10304/>
