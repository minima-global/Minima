#!/bin/sh

create-dmg \
  --volname "minima-app-1.101" \
  --volicon "minima.icns" \
  --app-drop-link 200 50 \
  --window-size 600 400 \
  --icon-size 100 \
  --hdiutil-verbose \
  --hide-extension "Minima.app" \
  --app-drop-link 450 50 \
  "MinimaApp.dmg" \
  /Users/abc/installer/source