import { context, type Span, SpanStatusCode, trace } from '@opentelemetry/api';
import type { Result } from 'postgraphile/grafserv';
import type { MiddlewareNext } from 'graphile-config';
import type { PromiseOrDirect } from 'grafast';

declare module 'grafast' {
  interface ExecutionExtra {
    /**
     * Add any attributes to the span for this execution.
     */
    span?: Span;
  }
}

const tracer = trace.getTracer('postgraphile-otel');

export const OTELPlugin: GraphileConfig.Plugin = {
  name: 'OTELPlugin',
  grafserv: {
    middleware: {
      processRequest(next, { requestDigest }) {
        // we'll need "preferJSON" to be set to true, so that grafserv
        // doesn't serialise the response to a string, preventing us
        // from correctly identifying errors in the response
        requestDigest.preferJSON = true;

        const parentSpan = trace.getActiveSpan();
        // @ts-expect-error
        const isHttpRequest = parentSpan?.['attributes']?.['http.method'];
        if (parentSpan && isHttpRequest) {
          return executeWithSpan(parentSpan, next, true);
        }

        return tracer.startActiveSpan('GraphQL Request', (span) =>
          executeWithSpan(span, next, false),
        );
      },
    },
  },
  grafast: {
    middleware: {
      execute(next, { args: { operationName, contextValue, requestContext } }) {
        const span = trace.getActiveSpan();
        if (!span) {
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
        return tracer.startActiveSpan('EstablishOperationPlan', (span) => {
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
        return tracer.startActiveSpan('ExecuteStep: ' + stepname, (span) => {
          event.executeDetails.extra.span = span;
          span.setAttribute('step.count', event.executeDetails.count);

          const ctx = trace.setSpan(context.active(), span);

          let result: ReturnType<typeof next>;
          try {
            result = context.with(ctx, next);
          } catch (err: any) {
            onSpanError(span, err, true);
          }

          if (Array.isArray(result)) {
            const bound = result.map((v) =>
              v instanceof Promise ? context.bind(ctx, v) : v,
            );
            void Promise.allSettled(bound.map((v) => Promise.resolve(v)))
              .catch((err) => onSpanError(span, err))
              .finally(() => span.end());
            return bound;
          }

          if (result instanceof Promise) {
            return context
              .bind(ctx, result)
              .catch((err) => onSpanError(span, err))
              .finally(() => span.end());
          }

          return result;
        });
      },
    },
  },
};

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

function executeWithSpan(
  span: Span,
  next: MiddlewareNext<PromiseOrDirect<Result | null>>,
  shouldEndSpan = false,
) {
  let rslt: ReturnType<typeof next>;
  try {
    rslt = next();
  } catch (err: any) {
    onSpanError(span, err, shouldEndSpan);
  }

  if (!(rslt instanceof Promise)) {
    if (shouldEndSpan) span.end();
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
          // @ts-expect-error
          message: obj.json.errors[0]?.['message'] || 'Unknown error',
        });
      }

      return obj;
    })
    .catch((err) => onSpanError(span, err))
    .finally(() => {
      if (shouldEndSpan) span.end();
    });
}
