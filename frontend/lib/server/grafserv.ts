import 'server-only';

import { createServerAdapter, useCORS as corsPlugin } from '@whatwg-node/server';
import { Readable } from 'node:stream';
import type { GrafservConfig, Result } from 'postgraphile/grafserv';
import { WhatwgGrafserv } from 'postgraphile/grafserv/whatwg-node__server/v4';

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
    return new Response(
      Readable.toWeb(stream) as ReadableStream,
      { status: result.statusCode, headers: result.headers },
    );
  }

  override createHandler() {
    return createServerAdapter(super.createHandler().handle, {
      plugins: [corsPlugin({ credentials: true, methods: ['GET', 'POST', 'OPTIONS'] })],
    });
  }
}

export const grafserv = (config: GrafservConfig) => new NextGrafserv(config);
