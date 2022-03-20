import queryString from "query-string";
import {Trip, GearItem, GearKind} from "./client.d";

export function postPuttrip(body: Trip, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<string> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "POST",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };

  options.body = JSON.stringify(body);

  let params = {};
  return (fetchFn || window.fetch)(`/putTrip` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        return response.json().then((json) => resolve(json));
      }
    });
  });
}

export function getSearchgearitems(q?: string, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<GearItem[]> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "GET",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };

  let params = {q};
  return (fetchFn || window.fetch)(`/searchGearItems` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        return response.json().then((json) => resolve(json));
      }
    });
  });
}

export function postDeletegearitem(body: string, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<boolean> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "POST",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };

  options.body = JSON.stringify(body);

  let params = {};
  return (fetchFn || window.fetch)(`/deleteGearItem` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        return response.json().then((json) => resolve(json));
      }
    });
  });
}

export function postPutgearitem(body: GearItem, fetchFn?: (input: RequestInfo, init?: RequestInit) => Promise<Response>): Promise<string> {
  let options: RequestInit = {
    credentials: "same-origin" as RequestCredentials,
    method: "POST",
    headers: {"Content-Type": "application/json;charset=utf-8"}
  };

  options.body = JSON.stringify(body);

  let params = {};
  return (fetchFn || window.fetch)(`/putGearItem` + "?" + queryString.stringify(params), options).then((response) => {
    return new Promise((resolve, reject) => {
      if (response.status !== 200) {
        return response.text().then((text) => reject({text, status: response.status}));
      } else {
        return response.json().then((json) => resolve(json));
      }
    });
  });
}
