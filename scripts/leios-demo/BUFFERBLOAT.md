# Introduction

TODO

## The Basic Problem of Bufferbloat

This section briefly explains the bufferfloat risk in the context of two Cardano nodes running Leios.
Bufferbloat has been an identified Internet infrastructural issue for over ten years.
Find more information at <http://www.bufferbloat.net>.

Consider the hops along the IP route between two Cardano nodes.
The hop with the (currently) least bandwidth is _the bottleneck_.

If the bottleneck uses well-configured Smart Queue Management (SQM, such as fq_codel or cake) and the Cardano nodes use TCP_NOTSENT_LOWAT (or equivalent), then no buffers between the two nodes will be bloated.
Therefore, the latency will never be artificially inflated.
If not, the latency will be inflated in either of the following two ways.

- If the bottleneck does not use well-configured SQM, then it will queue many bytes in its buffers when the two nodes exchange big Leios messages.
  All messages sent soon after, including Praos messages, will have increased latency due to being stuck in those buffers behind the queued Leios bytes.

- If the Cardano nodes don't use TCP_NOTSENT_LOWAT ([Cloudflare blog post](https://blog.cloudflare.com/http-2-prioritization-with-nginx/#tcp-send-buffers)), their own socket's send buffer might become bloated with _unsent_ bytes, even if the subsequent TCP logic is properly dequeuing those unsent bytes at the bottleneck's rate.

In other words, an idealized node doesn't write bytes to the socket any sooner than is useful.
The later you can send a message without actually delaying its arrival time---you're merely eliminating time it would spend waiting in a bloated buffer---the better, since that retains the opportunity to send a higher urgency message before sending more lower urgency messages.
Unfortunately, with today's Internet, it would be very difficult for the Cardano node to correctly predict when a later send would be as useful as an immediate send.

If the Cardano nodes used separate TCP streams for Leios and Praos, then fair queuing aspects of SQM (eg the `fq` in `fq_codel`) could (almost always) let the Praos bytes skip past however much Leios bytes are enqueued in any buffer.
However, that would be of no benefit for freshest-first within the Leios traffic.

## Challenges When Avoiding Bufferbloat

Even over ten years after the identification of the bufferbloat phenomenon within the Internet infrastructure, there is still no magic bullet that would let the Cardano developers reliably eliminate bufferbloat with no downsides.

- The large buffer capacities that risk allowing bufferbloat exist for a reason: they tend to improve bandwidth utilization when conditions aren't nominal.
  Ensuring the buffers are underutilized via SQM and TCP_NOTSENT_LOWAT therefore introduces risks of its own.
  TODO elaborate

- TCP_NOTSENT_LOWAT is not necessarily available on every platform Cardano intends to run on.
  There may be comparable alternatives; it's not yet determined.
  TODO determine it

- Because Cardano uses public Internet structure, there's no guarantee the bottleneck between two nodes is running well-configured SQM.
  Separate TCP streams for Praos and Leios might mitigate this somewhat, but only for Praos, not for Leios's freshest-first prioritization.

- One strategy to ensure proper SQM is to artificially induce a bottleneck at the sending node.
  Conceptually: measure the actual bandwidth to the other node and never send bytes faster than that.
  However, it's very difficult to measure that bandwidth reliably; that's what TCP congestion control algorithms are constantly attempting to do.
  It seems unwise to attempt to reimplement/abuse those algorithms, especially since the IP route may change to a different path at any moment.
  Thus, reliably establishing an artificial bottleneck is unfortunately likely to also underutilize the available bandwidth.
    - The Diffusion Layer's mux logic has some key similarities with the `fq` part of `fq_codel`.
      However, that mux cannot avoid bufferbloat etc unless that mux is the bottleneck along the connection to the peer.

- It's difficult to ensure our node has been tested against all relevant "classes" of IP routes, since they're the varying culmination of Internet-wide conditions.

- For a given buffer capacity, a higher bandwidth bottleneck will introduce less latency.
  However, higher bandwidth likely implies higher buffer capacity.

- For a burst of Leios traffic of a given size, higher bandwidth bottlenecks will introduce less latency.
  However, plausible protocol burst attacks are large enough to overwhelm anticipated bottleneck bandwidths between many Cardano nodes.
  The only compensation is that the larger the burst required, the longer the duration until the adversary will be elected enough times to be able to send another such burst.
