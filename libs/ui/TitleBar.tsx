import React from 'react';
import { cn } from '@app/ui/cn';
import { Heading } from './Heading';

type TitleBarProps = {
  title?: string | null;
  children?: React.ReactNode;
  className?: string;
};

export function TitleBar({ title, children,className}: TitleBarProps) {
  return (
    <div className={cn("my-4 flex gap-2 flex-wrap grow-0 h-min justify-between items-start", className)}>
      <Heading>{title}</Heading>
      <div className="flex items-center flex-wrap gap-2">{children}</div>
    </div>
  );
}
