-- Basic settings
vim.opt.termguicolors = true  -- Enable true color support
vim.opt.background = 'dark'   -- Set dark mode

-- Make sure lush.nvim is installed
local ensure_lush = function()
    local install_path = vim.fn.stdpath('data')..'/site/pack/packer/start/lush.nvim'
    if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
        vim.fn.system({'git', 'clone', '--depth', '1', 'https://github.com/rktjmp/lush.nvim', install_path})
        vim.cmd [[packadd lush.nvim]]
        return true
    end
    return false
end

ensure_lush()

-- Set the colorscheme
vim.cmd('colorscheme gptkod')
