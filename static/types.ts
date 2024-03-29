type GearKind = "Base" | "Technical" | "Clothing" | "Electronic" | "Nutrition";

type GearItem = IGearItem;

interface IGearItem {
  itemId: string;
  name: string;
  isPersonal: boolean;
  oz: number;
  kind: GearKind;
  creatorUserId: string;
}
