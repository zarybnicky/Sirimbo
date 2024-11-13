import React from 'react';
import { NextSeo } from 'next-seo';
import { typographyCls } from '@/ui/style';

type TitleBarProps = {
  title?: string | null;
  children?: React.ReactNode;
};

export function TitleBar({
  title,
  children,
  variant = 'heading',
  ...typography
}: TitleBarProps & Parameters<typeof typographyCls>[0]) {
  return (
    <div className="mb-4 flex gap-2 flex-wrap grow-0 h-min justify-between items-baseline relative">
      {title && variant === 'heading' && <NextSeo title={title} />}
      <h1
        className={typographyCls({
          spacing: variant === 'heading' ? 'topLevel' : 'default',
          ...typography,
        })}
      >
        {title}
      </h1>
      <div className="flex items-center flex-wrap gap-2">{children}</div>
    </div>
  );
}
