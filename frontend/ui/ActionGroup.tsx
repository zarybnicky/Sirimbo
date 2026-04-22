import React, { Suspense, useState } from 'react';
import { Dialog, DialogContent } from '@/ui/dialog';
import { Spinner } from '@/ui/Spinner';
import {
  DropdownMenu,
  DropdownMenuButton,
  DropdownMenuContent,
  DropdownMenuTrigger,
} from '@/ui/dropdown';
import { useConfirm } from '@/ui/Confirm';
import type { ResolvedAction } from '@/lib/actions';
import { buttonCls } from '@/ui/style';
import { cn } from '@/lib/cn';
import { ChevronDown, Plus } from 'lucide-react';

type BucketKey = 'add' | 'more';

const toArray = <T,>(v: T | readonly T[] | undefined): readonly T[] => {
  if (v === undefined) return [];
  if (Array.isArray(v)) return v;
  return [v] as readonly T[];
};

function partitionActions<Id extends string>(
  actions: readonly ResolvedAction<Id>[],
  {
    primary,
    groups,
    variant,
  }: {
    primary?: Id | readonly Id[];
    groups?: Partial<Record<'add', readonly Id[]>>;
    variant: 'toolbar' | 'row';
  },
): {
  primary: readonly ResolvedAction<Id>[];
  buckets: {
    key: BucketKey;
    items: readonly ResolvedAction<Id>[];
  }[];
} {
  if (variant === 'row') {
    return {
      primary: [],
      buckets: actions.length > 0 ? [{ key: 'more' as const, items: actions }] : [],
    };
  }

  const explicitPrimary = toArray(primary);
  const primarySet = new Set<Id>(
    explicitPrimary.length > 0
      ? explicitPrimary
      : actions.filter((x) => x.group === 'primary').map((x) => x.id),
  );
  const declaredGroups: Partial<Record<BucketKey, readonly Id[]>> = groups ?? {
    add: actions.filter((x) => x.group === 'add').map((x) => x.id),
  };

  const claimed = new Set<Id>(primarySet);
  const buckets: { key: BucketKey; items: ResolvedAction<Id>[] }[] = [];

  for (const [key, ids] of Object.entries(declaredGroups) as ['add', readonly Id[]][]) {
    const idSet = new Set(ids);
    const items = actions.filter((a) => !claimed.has(a.id) && idSet.has(a.id));
    for (const a of items) claimed.add(a.id);
    if (items.length > 0) buckets.push({ key, items });
  }

  const remaining = actions.filter((a) => !claimed.has(a.id));
  if (remaining.length > 0) buckets.push({ key: 'more', items: remaining });

  return {
    primary: actions.filter((a) => primarySet.has(a.id)),
    buckets,
  };
}

export function ActionGroup<Id extends string = string>({
  className,
  actions,
  primary,
  groups,
  variant = 'toolbar',
  align,
}: {
  className?: string;
  actions: readonly ResolvedAction<Id>[];
  primary?: Id | readonly Id[];
  groups?: Partial<Record<'add', readonly Id[]>>;
  variant?: 'toolbar' | 'row';
  align?: 'start' | 'end';
}) {
  const confirm = useConfirm();
  const [dialogAction, setDialogAction] = useState<Extract<
    ResolvedAction<Id>,
    { render: () => React.ReactNode }
  > | null>(null);
  const [pending, setPending] = useState<Id | null>(null);

  if (actions.length === 0) return null;

  const { primary: primaryActions, buckets } = partitionActions(actions, {
    primary,
    groups,
    variant,
  });

  const fire = async (action: ResolvedAction<Id>) => {
    if ('render' in action) return setDialogAction(action);
    if (action.confirm) {
      try {
        await confirm(
          typeof action.confirm === 'string'
            ? { description: action.confirm }
            : action.confirm,
        );
      } catch {
        return;
      }
    }
    setPending(action.id);
    try {
      await action.execute();
    } finally {
      setPending(null);
    }
  };

  const btn = (a: ResolvedAction<Id>) => (
    <button
      key={a.id}
      type="button"
      disabled={pending === a.id}
      className={buttonCls({ variant: 'outline', size: 'sm' })}
      onClick={() => fire(a)}
    >
      {a.icon && <a.icon className="size-4" />}
      {a.label}
    </button>
  );

  const dropdown = (key: BucketKey, items: readonly ResolvedAction<Id>[]) =>
    items.length > 0 ? (
      <DropdownMenu key={key}>
        {variant === 'row' ? (
          <DropdownMenuTrigger.RowDots className="relative top-0 right-0" />
        ) : key === 'add' ? (
          <DropdownMenuTrigger className={buttonCls({ variant: 'outline', size: 'sm' })}>
            <Plus className="size-5" />
            &thinsp;
            <ChevronDown className="size-5" />
          </DropdownMenuTrigger>
        ) : (
          <DropdownMenuTrigger.CornerDots className="relative top-0 right-0" />
        )}

        <DropdownMenuContent align={align ?? (variant === 'toolbar' ? 'end' : 'start')}>
          {items.map((a) => (
            <DropdownMenuButton
              key={a.id}
              disabled={pending === a.id}
              onSelect={() => fire(a)}
              className={a.variant === 'danger' ? 'bg-accent-3/70' : undefined}
            >
              {a.icon && <a.icon className="size-4" />}
              {a.label}
            </DropdownMenuButton>
          ))}
        </DropdownMenuContent>
      </DropdownMenu>
    ) : null;

  return (
    <>
      <div
        className={cn('inline-flex items-center gap-2', className)}
        onClick={(e) => e.stopPropagation()}
      >
        {primaryActions.map(btn)}
        {buckets.map(({ key, items }) => dropdown(key, items))}
      </div>

      {dialogAction && (
        <Dialog>
          <DialogContent {...dialogAction.dialogProps}>
            <Suspense fallback={<Spinner />}>{dialogAction.render()}</Suspense>
          </DialogContent>
        </Dialog>
      )}
    </>
  );
}
