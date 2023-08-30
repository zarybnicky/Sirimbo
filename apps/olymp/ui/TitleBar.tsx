import React from 'react';
import { cn } from '@app/ui/cn';
import { NextSeo } from 'next-seo';
import { typographyCls } from '@app/ui/style';

type TitleBarProps = {
  title?: string | null;
  children?: React.ReactNode;
  className?: string;
  noSeo?: boolean;
};

export function TitleBar({ title, children, className, noSeo }: TitleBarProps) {
  return (
    <div className={cn("mt-12 mb-4 flex gap-2 flex-wrap grow-0 h-min justify-between items-baseline", className)}>
      {title && !noSeo && <NextSeo title={title} />}
      <h1 className={typographyCls({ variant: 'heading' })}>
        {title}
      </h1>
      <div className="flex items-center flex-wrap gap-2">{children}</div>
    </div>
  );
}
