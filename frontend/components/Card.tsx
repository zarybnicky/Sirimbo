import classNames from "classnames";
import React from "react";

export const Card = (props: React.HTMLAttributes<HTMLDivElement>) => {
  return <div {...props} className={classNames(
    "bg-white overflow-hidden border border-slate-100 shadow-lg sm:rounded-lg p-3 mb-2",
    props.className
  )} />;
};