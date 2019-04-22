cd sticky_broadcasts
spatch --sp-file sticky_broadcasts.cocci FileDownloader.java > output.patch
cd ../set_text_size
spatch --sp-file set_text_size.cocci --iso lucid_browser.iso CustomWebView.java > output.patch
cd ../get_height
spatch --sp-file get_height.cocci TouchUtils.java > output.patch
cd ../get_color
spatch --sp-file get_color.cocci PushNotifications.java > output.patch
cd ../should_vibrate
spatch --sp-file should_vibrate.cocci IncomingRinger.java > output.patch
cd ../on_console_message
spatch --sp-file on_console_message.cocci ViewFileFragment.java > output.patch
cd ../get_drawable
spatch --sp-file get_drawable.cocci DisplayUtils.java SimpleListItemDividerDecoration.java > output.patch



