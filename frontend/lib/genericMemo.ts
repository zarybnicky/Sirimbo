import React from "react";

export const genericMemo: <T>(component: T) => T = React.memo;
