
public class Puzzle22 {

    enum Spell {
        MagicMissile, Drain, Shield, Poison, Recharge
    }

    public static void main(String[] args) {
        battle(10, 250, 13, 8);
    }

    private static void battle(int hp, int mana, int boss_hp, int boss_damage) {
        int minManaSpent = Integer.MAX_VALUE;

        while (true) {
            for (Spell spell : Spell.values()) {

            }
        }
    }
}
