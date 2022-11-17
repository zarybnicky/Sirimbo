import classNames from "classnames";
import React from "react";

export const Card = (props: React.HTMLAttributes<HTMLDivElement>) => {
  return <div {...props} className={classNames(
    "bg-white relative overflow-hidden border border-stone-200 shadow-md sm:rounded-xl p-3 mb-2",
    props.className
  )} />;
};
