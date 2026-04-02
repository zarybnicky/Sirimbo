import React from 'react';
import type { ResolvedActions } from '@/lib/actions';
import { cn } from '@/lib/cn';
import { ActionGroup } from '@/ui/ActionGroup';

export function ActionRow<TItem extends object = object>({
  actions,
  children,
  className,
}: {
  actions: ResolvedActions<TItem>;
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
