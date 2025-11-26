package discordlinker;

import net.kyori.adventure.text.format.NamedTextColor;
import org.bukkit.command.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class ReloadCommand implements CommandExecutor, TabCompleter {
    private final DiscordLinkPlugin plugin;

    public ReloadCommand(DiscordLinkPlugin plugin) {
        this.plugin = plugin;
    }

    @Override
    public boolean onCommand(CommandSender sender, Command command, String label, String[] args) {
        // permission check: OP or permission node
        if (!(sender.isOp() || sender.hasPermission("discordlinker.reload"))) {
            plugin.sendMessage(sender, NamedTextColor.RED, plugin.getMessage("reload.no_permission", "Nincs jogosultságod a reload parancshoz."));
            return true;
        }

        if (args.length == 0 || !"reload".equalsIgnoreCase(args[0])) {
            plugin.sendMessage(sender, NamedTextColor.YELLOW, plugin.getMessage("reload.usage", "Használat: /discordlinker reload"));
            return true;
        }

        // Perform reload and return a single result message to the caller
        DiscordLinkPlugin.ReloadResult res = plugin.reloadAll();
        plugin.sendMessage(sender, res.color, res.message);
        return true;
    }

    @Override
    public List<String> onTabComplete(CommandSender sender, Command command, String alias, String[] args) {
        if (args.length == 1) {
            List<String> opts = new ArrayList<>();
            opts.add("reload");
            return opts;
        }
        return Collections.emptyList();
    }
}
