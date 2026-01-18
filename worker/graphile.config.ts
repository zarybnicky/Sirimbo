import type { GraphileConfig } from "graphile-config";
import type {} from "graphile-worker";
import { initSDK } from '@hyperdx/node-opentelemetry';

initSDK({
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
