import * as React from 'react';

type HeadingProps = {
  children: string;
};

export const Heading = ({ children }: HeadingProps) => {
  return (
    <h1 className="mt-12 mb-8 text-5xl text-red-500 drop-shadow tracking-wide">
      {children}
    </h1>
  );
};
