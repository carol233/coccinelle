cd sticky_broadcasts
spatch --sp-file sticky_broadcasts.cocci android/ > project.patch
cd ../set_text_size
spatch --sp-file set_text_size.cocci --iso lucid_browser.iso Lucid-Browser/ > project.patch
cd ../get_color
spatch --sp-file get_color.cocci android-oss/ > project.patch
cd ../get_height
spatch --sp-file get_height.cocci glide/library > project.patch
cd ../should_vibrate
spatch --sp-file should_vibrate.cocci Signal-Android/ > project.patch
cd ../on_console_message
spatch --sp-file on_console_message.cocci MGit/ > project.patch
