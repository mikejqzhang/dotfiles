mkdir ~/.local/share/fonts

cp SFMono/* ~/.local/share/fonts
cp benton_sans_sv/* ~/.local/share/fonts
cp static/* ~/.local/share/fonts


fc-cache -f -v
