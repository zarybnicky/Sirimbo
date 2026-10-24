import { type Span, SpanStatusCode, trace } from '@opentelemetry/api';
import type { Result } from 'postgraphile/grafserv';
import { isPromiseLike } from 'postgraphile/grafast';
import { print } from 'postgraphile/graphql';

declare module 'postgraphile/grafast' {
  interface ExecutionExtra {
    /**
     * Add any attributes to the span for this execution.
     */
    span?: Span;
  }
}

const getTracer = () => trace.getTracer('postgraphile-otel');

export const OTELPlugin: GraphileConfig.Plugin = {
  name: 'OTELPlugin',
  grafserv: {
    middleware: {
      processRequest(next, { requestDigest }) {
        const parentSpan = trace.getActiveSpan();
        // @ts-expect-error Bad parent types
        const isHttpRequest = parentSpan?.attributes?.['http.method'];
        if (parentSpan && isHttpRequest) {
          return executeWithSpan(parentSpan, false);
        }

        return getTracer().startActiveSpan('GraphQL Request', executeWithSpan);

        function executeWithSpan(span: Span, owned: boolean = true) {
          if (!span.isRecording()) {
            try {
              return next();
            } finally {
              if (owned) span.end();
            }
          }

          // Recording request spans need the JSON result to inspect GraphQL errors.
          requestDigest.preferJSON = true;
          let rslt: ReturnType<typeof next>;
          try {
            rslt = next();
          } catch (err: any) {
            onSpanError(span, err, owned);
          }

          if (!(rslt instanceof Promise)) {
            if (owned) span.end();
            return rslt;
          }

          return rslt
            .then((obj: Result | null) => {
              if (obj?.type === 'error') {
                span.recordException(obj.error);
                span.setStatus({
                  code: SpanStatusCode.ERROR,
                  message: obj.error.message,
                });
              } else if (
                obj?.type === 'json' &&
                typeof obj.json === 'object' &&
                obj.json !== null &&
                'errors' in obj.json &&
                Array.isArray(obj.json.errors) &&
                obj.json.errors.length
              ) {
                span.setStatus({
                  code: SpanStatusCode.ERROR,
                  // @ts-expect-error Bad parent types
                  message: obj.json.errors[0]?.['message'] || 'Unknown error',
                });
              }

              return obj;
            })
            .catch((err) => onSpanError(span, err))
            .finally(() => {
              if (owned) span.end();
            });
        }
      },
    },
  },
  grafast: {
    middleware: {
      execute(next, { args: { operationName, contextValue, requestContext, document } }) {
        const span = trace.getActiveSpan();
        if (!span?.isRecording()) {
          return next();
        }

        let traceName = '';
        if (requestContext?.http) {
          traceName = `${requestContext.http.method} ${requestContext.http.path}`;
        }

        if (operationName) {
          traceName += ` (${operationName})`;
        }

        span.updateName(traceName);
        span.setAttribute('operation.name', operationName || 'unknown');
        span.setAttribute('operation.document', print(document));
        if (
          typeof contextValue === 'object' &&
          contextValue !== null &&
          'pgSettings' in contextValue &&
          typeof contextValue.pgSettings === 'object' &&
          contextValue.pgSettings !== null
        ) {
          for (const [key, value] of Object.entries(contextValue.pgSettings)) {
            if (typeof value === 'string') {
              span.setAttribute(`pg.${key}`, value);
            }
          }
        }

        return next();
      },
      establishOperationPlan(next) {
        return getTracer().startActiveSpan('EstablishOperationPlan', (span) => {
          try {
            return next();
          } catch (err: any) {
            onSpanError(span, err);
          } finally {
            span.end();
          }
        });
      },
      executeStep(next, event) {
        const stepname = Object.getPrototypeOf(event.step).constructor.name;
        return getTracer().startActiveSpan('ExecuteStep: ' + stepname, (span) => {
          if (!span.isRecording()) {
            try {
              return next();
            } finally {
              span.end();
            }
          }

          span.setAttribute('step.count', event.executeDetails.count);
          let rslt: ReturnType<typeof next>;
          try {
            rslt = next();
          } catch (err: any) {
            onSpanError(span, err, true);
          }

          if (Array.isArray(rslt)) {
            return traceStepResults(span, rslt);
          }

          if (isPromiseLike(rslt)) {
            return Promise.resolve(rslt).then(
              (items) => traceStepResults(span, items),
              (err) => onSpanError(span, err, true),
            );
          }

          span.end();
          return rslt;
        });
      },
    },
  },
};

function traceStepResults(span: Span, items: readonly unknown[]): readonly unknown[] {
  if (!items.some(isPromiseLike)) {
    span.end();
    return items;
  }

  let pending = 0;
  return items.map((item) => {
    if (!isPromiseLike(item)) return item;
    pending++;
    return Promise.resolve(item).then(
      (value) => {
        if (--pending === 0) span.end();
        return value;
      },
      (err) => {
        try {
          return onSpanError(span, err);
        } finally {
          if (--pending === 0) span.end();
        }
      },
    );
  });
}

function onSpanError(span: Span, err: any, end = false): never {
  span.recordException(err);
  span.setStatus({
    code: SpanStatusCode.ERROR,
    message: err.message,
  });
  if (end) {
    span.end();
  }
  throw err;
}
