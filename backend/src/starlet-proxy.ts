import proxy from 'express-http-proxy';
import type { Application } from 'express';
import cors from 'cors';

export const installStarletProxy = (app: Application) => {
  app.use(
    '/starlet/graphql',
    cors({
      origin: true,
      credentials: true,
    }),
    proxy('https://evidence.tsstarlet.com', {
      parseReqBody: true,
      proxyReqPathResolver(req) {
        if (req.body && typeof req.body === 'object' && req.body['query'] === '') {
          return '/spa_auth/login';
        }
        return '/graphql';
      },
      proxyReqOptDecorator(proxyReqOpts, srcReq) {
        const auth = srcReq.header('authorization') ?? '';
        const token = auth.replace(/^Bearer\s+/i, '').trim();
        delete proxyReqOpts.headers.authorization;
        proxyReqOpts.headers.cookie = token ? `auth=${encodeURIComponent(token)}` : '';

        proxyReqOpts.headers.host = 'evidence.tsstarlet.com';
        proxyReqOpts.headers.origin = 'https://evidence.tsstarlet.com';
        proxyReqOpts.headers.referer = 'https://evidence.tsstarlet.com';
        return proxyReqOpts;
      },
      proxyReqBodyDecorator(body) {
        if (body && typeof body === 'object' && body['query'] === '') {
          return body['variables'];
        }
        return body;
      },
    }),
  );
};
