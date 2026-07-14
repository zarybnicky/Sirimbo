import React, { type ComponentType, useMemo } from 'react';
import type { AuthState } from '@/ui/state/auth';
import { useAuth } from '@/ui/use-auth';
import { Client, TypedDocumentNode, useClient } from 'urql';
import { useRouter as useCompatRouter } from 'next/compat/router';
import { usePathname, useRouter as useAppRouter } from 'next/navigation';
import type { ConfirmOptions } from '@/ui/Confirm';
import { DialogContent } from '@/ui/dialog';

export type ActionRouter = {
  pathname: string | null;
  push: (href: string) => Promise<boolean> | void;
  replace: (href: string) => Promise<boolean> | void;
};

export type ActionContext<T> = {
  auth: AuthState;
  client: Client;
  router: ActionRouter;
  mutate: <D, V extends Record<string, unknown>>(
    doc: TypedDocumentNode<D, V>,
    vars: V,
  ) => Promise<D>;
  item: T;
};

type Resolvable<T, V> = V | ((ctx: ActionContext<T>) => V);
type Icon = ComponentType<{ className?: string }>;
type DialogBody<T> = ComponentType<ActionContext<T>>;
type Href = string;

const resolve = <T, V>(v: Resolvable<T, V>, ctx: ActionContext<T>): V =>
  typeof v === 'function' ? (v as (c: ActionContext<T>) => V)(ctx) : v;

export type Action<T, Id extends string = string> = {
  id: Id;
  label: Resolvable<T, string>;
  icon?: Resolvable<T, Icon>;
  visible?: Resolvable<T, boolean>;
  variant?: 'default' | 'danger';
  group?: 'primary' | 'add';
} & (
  | {
      confirm?: Resolvable<T, string | Partial<ConfirmOptions>>;
      execute: (ctx: ActionContext<T>) => Promise<void>;
    }
  | {
      render: DialogBody<T> | (() => Promise<{ default: DialogBody<T> }>);
      modal?: boolean;
      dialogProps?: React.ComponentPropsWithoutRef<typeof DialogContent>;
    }
  | {
      href: Resolvable<T, Href>;
    }
);

export type ResolvedAction<Id extends string = string> = {
  id: Id;
  label: string;
  icon?: Icon;
  variant?: 'default' | 'danger';
  group?: 'primary' | 'add';
} & (
  | {
      execute: () => Promise<void>;
      confirm?: string | Partial<ConfirmOptions>;
    }
  | {
      render: () => React.ReactNode;
      modal?: boolean;
      dialogProps?: React.ComponentPropsWithoutRef<typeof DialogContent>;
    }
  | {
      href: Href;
    }
);

export const defineActions =
  <T>() =>
  <const A extends readonly Action<T>[]>(
    actions: A,
  ): readonly Action<T, A[number]['id']>[] =>
    actions;

type IdOf<A extends readonly { id: string }[]> = A[number]['id'];

function resolveOne<T>(a: Action<T>, ctx: ActionContext<T>): ResolvedAction {
  const base = {
    id: a.id,
    label: resolve(a.label, ctx),
    icon: a.icon ? resolve(a.icon, ctx) : undefined,
    variant: a.variant,
    group: a.group,
  };

  if ('execute' in a) {
    return {
      ...base,
      confirm: a.confirm ? resolve(a.confirm, ctx) : undefined,
      execute: () => a.execute(ctx),
    };
  }

  if ('href' in a) {
    return {
      ...base,
      href: resolve(a.href, ctx),
    };
  }

  const Body =
    typeof a.render === 'function' && a.render.length === 0
      ? React.lazy(a.render as () => Promise<{ default: DialogBody<T> }>)
      : (a.render as DialogBody<T>);

  return {
    ...base,
    modal: a.modal,
    dialogProps: a.dialogProps,
    render: () => React.createElement(Body, ctx),
  };
}

function forItem<T, const A extends readonly Action<T>[]>(
  actions: A,
  ctx: ActionContext<T>,
): ResolvedAction<IdOf<A>>[] {
  return actions
    .filter((a) => resolve(a.visible, ctx) ?? true)
    .map((a) => resolveOne(a, ctx));
}

function useActionRouter(): ActionRouter {
  const router = useCompatRouter();
  const appRouter = useAppRouter();
  const pathname = usePathname();
  const asPath = router?.asPath;
  const isReady = router?.isReady;

  return useMemo(() => {
    if (router) {
      return {
        pathname: isReady ? (asPath ?? '').split(/[?#]/, 1)[0] || '/' : router.pathname,
        push: (href) => router.push(href),
        replace: (href) => router.replace(href),
      };
    }

    return {
      pathname: pathname,
      push: appRouter.push,
      replace: appRouter.replace,
    };
  }, [pathname, appRouter, asPath, isReady, router]);
}

function useBase() {
  const auth = useAuth();
  const client = useClient();
  const router = useActionRouter();
  return useMemo(() => {
    async function mutate<D, V extends Record<string, unknown>>(
      doc: TypedDocumentNode<D, V>,
      vars: V,
    ) {
      const r = await client.mutation(doc, vars).toPromise();
      if (r.error) throw r.error;
      return r.data as D;
    }
    return { auth, client, router, mutate };
  }, [auth, client, router]);
}

export function useActions<T extends object, Ids extends string>(
  actions: readonly Action<T, Ids>[],
  item: T | null | undefined,
): ResolvedAction<Ids>[] {
  const base = useBase();

  return useMemo(
    () => (item ? forItem(actions, { ...base, item }) : []),
    [actions, base, item],
  );
}

export function useActionMap<T extends { id: string }, Ids extends string>(
  actions: readonly Action<T, Ids>[],
  items: readonly T[],
): Map<string, ResolvedAction<Ids>[]> {
  const base = useBase();

  return useMemo(
    () => new Map(items.map((item) => [item.id, forItem(actions, { ...base, item })])),
    [actions, base, items],
  );
}
