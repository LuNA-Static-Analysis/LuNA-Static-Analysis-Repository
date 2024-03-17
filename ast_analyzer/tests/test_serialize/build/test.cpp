#include "ucenv.h"

extern "C" void c_init(int, OutputDF &); // as init

// MAIN
BlockRetStatus block_0(CF &self)
{

	self.NextBlock=1;
	return CONTINUE;
}

// STRUCT: sub main()
BlockRetStatus block_1(CF &self)
{
	Id _id_0=self.create_id(); // x

// GEN BODY: sub main()
	{ // FORK_BI: cf _l4: init(1, x);
		CF *child=self.fork(2);
		child->id(0)=_id_0;
	}

	return EXIT;
}

// BI_EXEC: cf _l4: init(1, x);
BlockRetStatus block_2(CF &self)
{
	if (self.migrate(CyclicLocator(0))) {
		return MIGRATE;
	}

	{
		DF _out_0;
		// EXEC_EXTERN cf _l4: init(1, x);
		c_init(
			// int 1
			1, 
			// name x
			_out_0);

		{
			DF stored=_out_0;
			self.store(self.id(0), stored);
		}
	}

	// req_unlimited: x
	{		DF posted=self.wait(self.id(0));
	self.post(self.id(0), posted, CyclicLocator(0), -1);
	}
	return EXIT;
}

extern "C" void init_blocks(BlocksAppender add)
{
	bool ok=true;

	ok = ok && add(block_0)==0;
	ok = ok && add(block_1)==1;
	ok = ok && add(block_2)==2;

	assert(ok);
}

