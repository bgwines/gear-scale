export type GearKind = "Base" | "Technical" | "Clothing" | "Electronic" | "Nutrition";

export type GearItem = IGearItem;

interface IGearItem {
  itemId: string;
  name: string;
  isPersonal: boolean;
  oz: number;
  kind: GearKind;
  creatorUserId: string;
}

export type Trip = ITrip;

interface ITrip {
  tripId: string;
  tripName: string;
  tripCreatorUserId: string;
  tripMemberUserIdsCommaSeparated: string;
}