import { type ComponentType, useMemo } from 'react';
import { type AuthState } from '@/ui/state/auth';
import { useAuth } from '@/ui/use-auth';
import { Client, TypedDocumentNode, useClient } from 'urql';
import { NextRouter, useRouter } from 'next/router';
import { DialogContent } from '@radix-ui/react-dialog';
import type { ConfirmOptions } from '@/ui/Confirm';

export type ActionContext<TItem> = {
  auth: AuthState;
  client: Client;
  router: NextRouter;
  mutate: <TData, TVars extends Record<string, unknown>>(
    document: TypedDocumentNode<TData, TVars>,
    variables: TVars,
  ) => Promise<TData>;
  item: TItem;
};

type ActionIcon = ComponentType<{ className?: string }>;

type Resolvable<TItem, TValue> = TValue | ((ctx: ActionContext<TItem>) => TValue);

type BaseAction<TItem> = {
  id: string;
  primary?: boolean;
  visible: (ctx: ActionContext<TItem>) => boolean;
  label: Resolvable<TItem, string>;
  icon?: Resolvable<TItem, ActionIcon>;
  variant?: 'default' | 'danger';
};

type ResolvedBaseAction<TItem> = Omit<BaseAction<TItem>, 'visible' | 'label' | 'icon'> & {
  label: string;
  icon?: ActionIcon;
};

export type DialogAction<TItem> = BaseAction<TItem> & {
  type: 'dialog';
  render: (ctx: ActionContext<TItem>) => React.ReactNode;
  dialogProps?: React.ComponentPropsWithoutRef<typeof DialogContent>;
};

export type MutationAction<TItem> = BaseAction<TItem> & {
  type: 'mutation';
  confirm?: Resolvable<TItem, string | Partial<ConfirmOptions>>;
  execute: (ctx: ActionContext<TItem>) => Promise<void>;
};

export type Action<TItem> = DialogAction<TItem> | MutationAction<TItem>;

export type ResolvedDialogAction<TItem> = ResolvedBaseAction<TItem> &
  Omit<DialogAction<TItem>, keyof BaseAction<TItem>>;

export type ResolvedMutationAction<TItem> = ResolvedBaseAction<TItem> &
  Omit<MutationAction<TItem>, keyof BaseAction<TItem> | 'confirm'> & {
    confirm?: string | Partial<ConfirmOptions>;
  };

export type ResolvedAction<TItem> =
  | ResolvedDialogAction<TItem>
  | ResolvedMutationAction<TItem>;

export type ResolvedActions<TItem extends object = object> = {
  all: ResolvedAction<TItem>[];
  primary: ResolvedAction<TItem>[];
  secondary: ResolvedAction<TItem>[];
  ctx: ActionContext<TItem>;
};

const resolve = <TItem, TValue>(
  v: Resolvable<TItem, TValue>,
  ctx: ActionContext<TItem>,
): TValue =>
  typeof v === 'function' ? (v as (ctx: ActionContext<TItem>) => TValue)(ctx) : v;

function resolveAction<TItem>(
  action: Action<TItem>,
  ctx: ActionContext<TItem>,
): ResolvedAction<TItem> {
  const base: ResolvedBaseAction<TItem> = {
    ...action,
    label: resolve(action.label, ctx),
    icon: action.icon ? resolve(action.icon, ctx) : undefined,
  };

  if (action.type === 'mutation') {
    return {
      ...base,
      type: 'mutation',
      confirm: action.confirm ? resolve(action.confirm, ctx) : undefined,
      execute: action.execute,
    };
  }

  return {
    ...base,
    type: 'dialog',
    render: action.render,
    dialogProps: action.dialogProps,
  };
}

export function useActions<TItem extends object>(
  actions: Action<TItem>[],
  item: TItem | null | undefined,
): ResolvedActions<TItem> {
  const auth = useAuth();
  const client = useClient();
  const router = useRouter();

  return useMemo(() => {
    async function mutate<TData, TVars extends Record<string, unknown>>(
      document: TypedDocumentNode<TData, TVars>,
      variables: TVars,
    ): Promise<TData> {
      const result = await client.mutation(document, variables).toPromise();
      if (result.error) throw result.error;
      return result.data!;
    }
    const ctx = { auth, client, router, item: item!, mutate };

    const visible = !item
      ? []
      : actions.filter((a) => a.visible(ctx)).map((a) => resolveAction(a, ctx));

    return {
      all: visible,
      primary: visible.filter((a) => !!a.primary),
      secondary: visible.filter((a) => !a.primary),
      ctx,
    };
  }, [actions, auth, client, item, router]);
}

export function useActionMap<TItem extends { id: string }>(
  actions: Action<TItem>[],
  items: TItem[],
): Map<string, ResolvedActions<TItem>> {
  const auth = useAuth();
  const client = useClient();
  const router = useRouter();

  return useMemo(() => {
    const map = new Map<string, ResolvedActions<TItem>>();

    async function mutate<TData, TVars extends Record<string, unknown>>(
      document: TypedDocumentNode<TData, TVars>,
      variables: TVars,
    ): Promise<TData> {
      const result = await client.mutation(document, variables).toPromise();
      if (result.error) throw result.error;
      return result.data!;
    }
    for (const item of items) {
      const ctx: ActionContext<TItem> = { auth, client, item, router, mutate };

      const visible = actions
        .filter((a) => a.visible(ctx))
        .map((a) => resolveAction(a, ctx));

      map.set(item.id, {
        all: visible,
        primary: visible.filter((a) => !!a.primary),
        secondary: visible.filter((a) => !a.primary),
        ctx,
      });
    }

    return map;
  }, [actions, auth, client, items, router]);
}
