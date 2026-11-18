import { type Span, SpanStatusCode, trace } from '@opentelemetry/api';
import { isPromiseLike } from 'postgraphile/grafast';
import { getOperationAST, print } from 'postgraphile/graphql';

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
  grafast: {
    middleware: {
      execute(next, { args: { operationName, contextValue, document } }) {
        const name = operationName ?? getOperationAST(document)?.name?.value;
        return getTracer().startActiveSpan(name ? `GraphQL ${name}` : 'GraphQL', async (span) => {
          try {
            if (span.isRecording()) {
              span.setAttribute('operation.name', name ?? 'unknown');
              span.setAttribute('operation.document', print(document));
              if (
                typeof contextValue === 'object' &&
                contextValue !== null &&
                'pgSettings' in contextValue &&
                typeof contextValue.pgSettings === 'object' &&
                contextValue.pgSettings !== null
              ) {
                for (const [key, value] of Object.entries(contextValue.pgSettings)) {
                  if (typeof value === 'string') span.setAttribute(`pg.${key}`, value);
                }
              }
            }

            const result = await next();
            if ('errors' in result && result.errors?.length) {
              span.setStatus({ code: SpanStatusCode.ERROR, message: result.errors[0]!.message });
            }
            return result;
          } catch (err: any) {
            onSpanError(span, err);
          } finally {
            span.end();
          }
        });
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
