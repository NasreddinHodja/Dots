-- Options (converted from init.vim)
vim.opt.showmatch = true            -- show matching bracket
vim.opt.ignorecase = true           -- case insensitive search
vim.opt.hlsearch = true             -- highlight search
vim.opt.incsearch = true            -- incremental search
vim.opt.tabstop = 2                 -- number of columns occupied by a tab
vim.opt.softtabstop = 2             -- see multiple spaces as tabstops so <BS> does the right thing
vim.opt.shiftwidth = 2              -- width for autoindents
vim.opt.autoindent = true           -- indent a new line the same amount as the line just typed
vim.opt.relativenumber = true       -- add line numbers
vim.opt.colorcolumn = "80"          -- set an 80 column border for good coding style
vim.opt.mouse = "a"                 -- enable mouse click
vim.opt.clipboard = "unnamedplus"   -- using system clipboard
vim.opt.termguicolors = true

-- Plugins (lazy.nvim)
local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.uv.fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({
	spec = {
		{ import = "plugins" },
	},
	install = { colorscheme = { "base16-nvim" } },
})
