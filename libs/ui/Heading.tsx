import * as React from 'react';

type HeadingProps = {
  children?: string | null;
};

export const Heading = ({ children }: HeadingProps) => {
  return (
    <h1 className="mt-12 mb-8 text-4xl text-accent-12 drop-shadow tracking-wide">
      {children}
    </h1>
  );
};
