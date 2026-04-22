import React from 'react';
import type { ResolvedAction } from '@/lib/actions';
import { cn } from '@/lib/cn';
import { ActionGroup } from '@/ui/ActionGroup';

export function ActionRow<Id extends string = string>({
  actions,
  children,
  className,
}: {
  actions: ResolvedAction<Id>[];
  children: React.ReactNode;
  className?: string;
}) {
  return (
    <div className={cn('flex gap-3 mb-1 items-center', className)}>
      <ActionGroup variant="row" actions={actions} />
      {children}
    </div>
  );
}
