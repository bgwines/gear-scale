export interface IGearItem {
  itemId: string;
  name: string;
  isPersonal: boolean;
  oz: number;
  kind: GearKind;
  creatorUserId: string;
}

export interface ITrip {
  tripId: string;
  tripName: string;
  tripCreatorUserId: string;
  tripMemberUserIdsCommaSeparated: string;
}

export type GearItem = IGearItem;

export type GearKind = "Base" | "Technical" | "Clothing" | "Electronic" | "Nutrition";

export type Trip = ITrip;
