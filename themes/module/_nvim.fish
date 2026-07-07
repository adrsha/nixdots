# module/_nvim.fish — reload running nvim instances and recompile base46 cache

# Write a lua env-patch file so running nvim instances can update their
# stale vim.env before rebuilding the theme.
set nvim_patch_file /tmp/nvim_theme_patch.lua

printf 'vim.env.THEME_NAME    = "%s"\n' $THEME_NAME  > $nvim_patch_file
printf 'vim.env.THEME_VARIANT = "%s"\n' $THEME_VARIANT >> $nvim_patch_file

for key in (set --names | string match -r '^THEME_')
    set -l short (string replace 'THEME_' '' $key)
    printf 'vim.env["THEME_%s"] = "%s"\n' $short $$key >> $nvim_patch_file
end

# Reload running nvim instances via their sockets.
set -l runtime_dir $XDG_RUNTIME_DIR
if test -z "$runtime_dir"
    set runtime_dir /run/user/(id -u)
end

for s in $runtime_dir/nvim.*/0
    if test -S "$s"; and test "$s" != "$NVIM_LISTEN_ADDRESS"
        nvim --server $s --remote-expr 'v:lua.reload_nvchad_theme()' >/dev/null 2>&1
        if test $status -eq 0
            echo "set_theme.fish: reloaded nvim at $s"
        else
            echo "set_theme.fish: could not reload nvim at $s" >&2
        end
    end
end

# Recompile the base46 highlight cache so the next nvim launch is correct.
nvim --headless -c 'lua require("base46").compile()' -c 'qa' 2>/dev/null
if test $status -eq 0
    echo "set_theme.fish: recompiled base46 cache"
else
    echo "set_theme.fish: base46 cache recompile failed" >&2
end
