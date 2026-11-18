import { getGraphqlHandler } from '@/lib/server/graphql';

export const runtime = 'nodejs';

export function POST(request: Request) {
  return getGraphqlHandler().fetch(request);
}

export { POST as GET, POST as OPTIONS };
