import { getGraphqlServer } from '@/lib/server/graphql';

export const runtime = 'nodejs';

export function POST(request: Request) {
  return getGraphqlServer().handler.fetch(request);
}

export { POST as GET, POST as OPTIONS };
