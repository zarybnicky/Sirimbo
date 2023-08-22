import React from 'react';
import { cn } from '@app/ui/cn';
import { NextSeo } from 'next-seo';
import { typographyCls } from '@app/ui/style';

type TitleBarProps = {
  title?: string | null;
  children?: React.ReactNode;
  className?: string;
};

export function TitleBar({ title, children,className}: TitleBarProps) {
  return (
    <div className={cn("my-4 flex gap-2 flex-wrap grow-0 h-min justify-between items-baseline", className)}>
      {title && <NextSeo title={title} />}
      <h1 className={typographyCls({ variant: 'heading', className: 'mt-12 mb-8' })}>
        {title}
      </h1>
      <div className="flex items-center flex-wrap gap-2">{children}</div>
    </div>
  );
}
