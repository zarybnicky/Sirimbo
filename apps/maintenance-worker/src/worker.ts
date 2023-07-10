export default {
  async fetch(request: Request, env: Env, ctx: ExecutionContext): Promise<Response> {
    return new Response('Na webu právě probíhá údržba, zkuste to prosím znovu za 30-90min. Děkuji za pochopení.');
  },
};
