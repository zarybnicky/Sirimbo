import type { GraphileConfig } from "graphile-config";
import type {} from "graphile-worker";
import * as HyperDX from '@hyperdx/node-opentelemetry';

HyperDX.init({
  consoleCapture: false,
  service: 'rozpisovnik-worker',
});

const preset: GraphileConfig.Preset = {
  worker: {
    connectionString: process.env.DATABASE_URL,
    concurrentJobs: 5,
    fileExtensions: [".js", ".cjs", ".mjs", ".ts", ".cts", ".mts"],
  },
};

export default preset;
