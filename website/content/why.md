---
title: Why Harding?
---

## The Story Behind Harding Smalltalk

In the world of computer science, few people have shaped modern computing as profoundly as **Daniel Ingalls**. He is my number one hero of computer science—nice, humble, incredibly sharp, with zero ego and always friendly and helpful. Dan Ingalls set the tone for the entire Smalltalk community through decades of work, particularly during the Squeak years.

The story of Harding Smalltalk begins with a book that Dan Ingalls liked when he was young: Jules Verne's *The Mysterious Island*. In that novel, a group of castaways stranded on an isolated land must rely on their engineering ingenuity and collaborative spirit to survive and ultimately escape. When Smalltalk-80 was ready to leave Xerox PARC—when the ideas that had been nurtured in that legendary research environment were finally ready for the wider world—Dan Ingalls chose the balloon from *The Mysterious Island* as the cover image for Byte magazine. The balloon represented escape, liberation, the moment when ideas finally break free from their island and take flight. That image was a perfect metaphor for what Dan and the team accomplished: taking the revolutionary concepts of Smalltalk and letting them soar beyond the confines of PARC to influence generations of programmers and countless modern programming languages.

The protagonist of Verne's novel, **Cyrus Harding**, is an engineer and leader whose practical wisdom, calm determination, and ability to solve impossible problems guide the castaways through their ordeal. In naming Harding Smalltalk, I found a parallel. Dan Ingalls played a remarkably similar role in the Smalltalk story—he is the engineer who implemented visions, who solved the hard technical problems that made Smalltalk possible, who later also guided the Squeak community. Like Harding leading the colonists, Dan led the Smalltalk community, always with generosity and a helpful hand. This project is my tribute to him.

## The Vision

Harding Smalltalk is not a highly ambitious undertaking in the traditional sense. It is, first and foremost, a **fun and open experiment**—a personal exploration driven by curiosity. I wanted to see if Smalltalk, this beautiful and influential language, might still have something unique to offer in the landscape of 2026, particularly as AI-assisted development transforms how we write software. The goal is modest but practical: to create a Smalltalk that feels familiar to those who love the language while embracing the realities of modern development workflows.

## Philosophy

### Meeting Developers Where They Are

The guiding philosophy of Harding Smalltalk centers on meeting developers where they are. Today's tooling landscape is built around files, version control systems, and command-line workflows that many programmers find essential. At the same time, the essence of Smalltalk—the immediacy, the aliveness, the deep integration between programmer and image—remains a compelling vision of what programming could be.

Harding Smalltalk attempts to bridge these experiences. You write your code in files, you commit to Git, you build with familiar toolchains—but you also have a Smalltalk heart beating. The image concept is not used, but the experience aims to be the same, alive and responsive. You can inspect, you can change, you can experiment, all while maintaining the workflow that modern development demands.

### Native Performance

The technical foundation of Harding Smalltalk is **Nim**, a language that compiles to C and offers native performance while providing a modern, expressive syntax. By building on Nim and its integration with C compilation toolchains, Harding Smalltalk can leverage the entire ecosystem of C libraries, the optimizations of modern compilers, and the deployment options that native compilation provides. Hopefully Harding can be used for real work, and interop with the systems that power modern software infrastructure.

### Concurrency Vision

Looking toward new ground, Harding Smalltalk embraces native thread support with a planned actor-like shared-nothing architecture. In this model, each actor runs in its own thread with its own isolated state, communicating through message passing. This approach eliminates the complexity and fragility of shared-memory concurrency while giving developers the performance benefits of true parallelism. It feels like a natural fit for Smalltalk's message-passing heritage—an embodiment of Alan Kay's original vision that objects should communicate only through messaging, never by sharing state directly.

At this moment though, the current VM offers "green" Processes, very much like classic Smalltalk implementations have. However, each Process in Harding has its own stackless VM, paving the path forward.

### AI-Ready

And of course, there is AI. As we look toward 2026, the landscape of software development is being completely reshaped. Harding Smalltalk aims to provide hooks and abstractions that make it natural to integrate AI assistance into the development workflow. Whether this means AI agents that help refactor code, systems that suggest improvements, or entirely new ways of interacting with programs during development, Harding Smalltalk aims to be a platform where these possibilities can be explored.

## A Tribute

This is my experiment, my tribute, my small contribution to a tradition that Dan Ingalls helped create. I hope it brings something valuable to the Smalltalk community and perhaps finds its way into the hands of developers who share my admiration for Dan's work and my warm feelings around Smalltalk.

---

*Harding Smalltalk — An Introduction*
