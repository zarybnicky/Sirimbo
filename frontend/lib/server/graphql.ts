import 'server-only';

import { getRequestContext } from '@/lib/server/tenant';
import { execute, hookArgs } from 'postgraphile/grafast';
import type { TypedDocumentNode } from '@graphql-typed-document-node/core';
import {
  type DocumentNode,
  getOperationAST,
  type GraphQLSchema,
  validate,
} from 'graphql';
import path from 'node:path';
import { postgraphile, type PostGraphileInstance } from 'postgraphile';
import sharedPreset from '@rozpisovnik/graphql/graphile.config';
import { makePgService } from 'postgraphile/adaptors/pg';
import { getPool } from './postgresql';
import { WhatwgGrafserv } from 'postgraphile/grafserv/whatwg-node__server/v4';
import { Readable } from 'node:stream';
import type { GrafservConfig, Result } from 'postgraphile/grafserv';
import { createServerAdapter, useCORS as corsPlugin } from '@whatwg-node/server';

declare global {
  var graphqlServer:
    | {
        instance: PostGraphileInstance;
        handler: ReturnType<NextGrafserv['createHandler']>;
      }
    | undefined;
}

// Next re-evaluates this module when server code changes; keep the pool, not the old schema.
if (process.env.NODE_ENV === 'development') {
  const instance = globalThis.graphqlServer?.instance;
  const services = instance?.getResolvedPreset().pgServices ?? [];
  globalThis.graphqlServer = undefined;
  void Promise.all([
    instance?.release(),
    ...services.map((service) => service.release?.()),
  ])
    // eslint-disable-next-line unicorn/prefer-top-level-await -- Schema initialization can still be waiting for the database.
    .catch((error) =>
      console.error('Failed to release PostGraphile during hot reload', error),
    );
}

export function getGraphqlServer() {
  if (globalThis.graphqlServer) return globalThis.graphqlServer;
  const instance = postgraphile({
    extends: [sharedPreset],
    grafserv: { graphiqlStaticPath: '/graphql/ruru-static/' },
    pgServices: [
      makePgService({
        schemas: ['public', 'federated', 'crawler'],
        pool: getPool(),
        superuserPool: getPool(),
      }),
    ],
    schema: {
      exportSchemaSDLPath:
        process.env.NODE_ENV === 'development'
          ? path.resolve('../schema.graphql')
          : undefined,
    },
    grafast: {
      async context(ctx) {
        const request = ctx.whatwgv0?.request;
        if (!request) return {};
        const { pgSettings } = await getRequestContext(undefined, request);
        if (
          process.env.NODE_ENV === 'development' &&
          request.headers.get('x-elevated') === '1'
        ) {
          return { pgSettings: { ...pgSettings, role: 'system_admin' } };
        }
        return { pgSettings };
      },
    },
  });
  return (globalThis.graphqlServer = {
    instance,
    handler: instance.createServ((config) => new NextGrafserv(config)).createHandler(),
  });
}

const validatedDocuments = new WeakMap<GraphQLSchema, WeakSet<DocumentNode>>();

export async function executeGraphql<
  TResult,
  TVariables extends Record<string, unknown> = Record<string, never>,
>(
  document: TypedDocumentNode<TResult, TVariables>,
  variables?: TVariables,
  context?: { pgSettings: Record<string, string> },
) {
  const { pgSettings } = context ?? (await getRequestContext());
  const { schema, resolvedPreset } = await getGraphqlServer().instance.getSchemaResult();
  let documents = validatedDocuments.get(schema);
  if (!documents) validatedDocuments.set(schema, (documents = new WeakSet()));
  if (!documents.has(document)) {
    const errors = validate(schema, document);
    if (errors.length > 0)
      throw new Error(errors.map((error) => error.message).join('\n'));
    documents.add(document);
  }
  const args = await hookArgs({
    schema,
    resolvedPreset,
    document,
    variableValues: variables ?? {},
    operationName: getOperationAST(document)?.name?.value,
  });
  Object.assign(args.contextValue, { pgSettings });
  const result = await execute(args);
  if (Symbol.asyncIterator in result)
    throw new Error('Streaming GraphQL results are not supported here.');
  if (result.errors?.length) {
    throw new Error(result.errors.map((error) => error.message).join('\n'));
  }
  if (!result.data) {
    throw new Error('GraphQL request returned no data.');
  }
  // Grafast returns null-prototype objects; React requires plain objects at client boundaries.
  return structuredClone(result.data) as TResult;
}

class NextGrafserv extends WhatwgGrafserv {
  private readonly streams = new Set<Readable>();

  constructor(config: GrafservConfig) {
    super(config);
    this.onRelease(() => {
      for (const stream of this.streams) stream.destroy();
      this.streams.clear();
    });
  }

  protected override grafservResponseToWhatwg(result: Result | null): Response {
    if (result?.type !== 'bufferStream') {
      return super.grafservResponseToWhatwg(result);
    }

    // Grafserv's WHATWG adapter does not yet convert its schema-watch SSE stream.
    const stream = Readable.from(result.bufferIterator, { objectMode: false });
    this.streams.add(stream);
    stream.once('close', () => this.streams.delete(stream));
    return new Response(Readable.toWeb(stream) as ReadableStream, {
      status: result.statusCode,
      headers: result.headers,
    });
  }

  override createHandler() {
    return createServerAdapter(super.createHandler().handle, {
      plugins: [corsPlugin({ credentials: true, methods: ['GET', 'POST', 'OPTIONS'] })],
    });
  }
}
