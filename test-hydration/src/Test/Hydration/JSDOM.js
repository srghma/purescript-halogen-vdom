import { JSDOM } from "jsdom";

export const makeImpl = content => new JSDOM(content)
export const windowImpl = jsdom => jsdom.window;
export const serializeImpl = jsdom => jsdom.serialize();
