import React from "react";
import { getCookie, setCookies } from "cookies-next";

export const useCookie = (key: string, defaultValue: string | undefined = undefined) => {
  const [cookie, setCookie] = React.useState<string | undefined>((getCookie(key) as string) ?? defaultValue);

  const updateCookie = (value: string, numberOfDays: number) => {
    const expires = new Date();
    expires.setTime(expires.getTime() + numberOfDays * 60 * 60 * 24 * 1000);

    setCookie(value);
    setCookies(key, value, { path: '/', expires });
  };

  return [cookie, updateCookie] as const;
};
