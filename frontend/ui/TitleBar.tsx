import React from 'react';
import { NextSeo } from 'next-seo';
import { typographyCls } from '@/ui/style';

type TitleBarProps = {
  title?: string | null;
  children?: React.ReactNode;
};

export function TitleBar({ title, children, ...typography }: TitleBarProps & Parameters<typeof typographyCls>[0]) {
  const isPageHeader = !typography.variant || typography.variant === 'heading';
  const baseCss = "mb-4 flex gap-2 flex-wrap grow-0 h-min justify-between items-baseline relative" + (isPageHeader ? ' mt-12' : '');

  return (
    <div className={baseCss}>
      {(!typography?.variant || typography.variant === 'heading') && title && (
        <NextSeo title={title} />
      )}
      <h1 className={typographyCls(typography)}>
        {title}
      </h1>
      <div className="flex items-center flex-wrap gap-2">{children}</div>
    </div>
  );
}
